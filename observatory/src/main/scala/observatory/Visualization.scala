package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{sin, cos, acos, abs, toRadians}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def weight(x: Location, xi: Location) = 1.0D / math.pow(distance(x, xi), 2.0D)

    val toleranceDistance = 1.0D //km

    temperatures.filter{ case (p, temp) => distance(location, p) <= toleranceDistance }
                .headOption match {
      case Some((p, temp)) => temp
      case None => temperatures.map{ case (p, temp) => weight(location, p) * temp }.sum /
                   temperatures.map{ case (p, temp) => weight(location, p) }.sum
    }
  }

  /**
   * The distance between two points in [km]
   */
  def distance(p1: Location, p2: Location): Double = {
    def antipodes(p1: Location, p2: Location) = 
      p1 == Location(-p2.lat, p2.lon + 180.0D) || p1 == Location(-p2.lat, p2.lon - 180.0D)

    def centralAngle(p1: Location, p2: Location) =
      if (p1 == p2) 0.0D
      else if (antipodes(p1, p2)) math.Pi
      else acos(
        sin(toRadians(p1.lat)) * sin(toRadians(p2.lat)) 
        + cos(toRadians(p1.lat)) * cos(toRadians(p2.lat)) * cos(abs(toRadians(p1.lon) - toRadians(p2.lon)))
      )

    val earthRadius = 6371.0D // km

    earthRadius * centralAngle(p1, p2)
  }


  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  type Candidate = (Temperature, Color)
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val exact = points.filter{ case (temp, color) => temp == value }
    val lowerPoints = points.filter{ case (temp, color) => temp < value }
    val upperPoints = points.filter{ case (temp, color) => temp > value }

    if (!exact.isEmpty) exact.head._2
    else if (lowerPoints.isEmpty) points.minBy(_._1)._2
    else if (upperPoints.isEmpty) points.maxBy(_._1)._2
    else {
      val lower = lowerPoints.minBy{ case (temp, color) => value - temp }
      val upper = upperPoints.minBy{ case (temp, color) => temp - value }

      val scale = (value - lower._1).toDouble / (upper._1 - lower._1).toDouble

      def interpolateChannel(lower: Int, upper: Int): Int = {
        val delta = abs(upper - lower) * scale 
        math.round(if (lower <= upper) lower.toDouble + delta else lower.toDouble - delta).toInt
      }

      Color(
        interpolateChannel(lower._2.red, upper._2.red),
        interpolateChannel(lower._2.green, upper._2.green),
        interpolateChannel(lower._2.blue, upper._2.blue)
      )
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180

    def pixelIndex(loc: Location) =
      math.round(loc.lon + 180.0D) + math.round(179.0D - (loc.lat + 90.0D)) * width

    val pixels = Array.fill(width * height)(com.sksamuel.scrimage.Color(0, 0, 0).toPixel)

    temperatures.foreach{ case (location, temperature) =>
      val color = interpolateColor(colors, temperature)
      val pixel = com.sksamuel.scrimage.Color(color.red, color.green, color.blue).toPixel

      pixels(pixelIndex(location).toInt) = pixel
    }
    
    Image(width, height, pixels) 
  }
}


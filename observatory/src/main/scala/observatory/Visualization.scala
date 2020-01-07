package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{sqrt, pow, sin, asin, cos, acos, abs, toRadians}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  //var i = 0
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val toleranceDistance = 1.0D //km

    val tempsWithDistances = temperatures.map{ case (loc, temp) =>
      val dist = distance(loc, location)
      val weight = 1.0D / math.pow(dist, 6.0D)
      (loc, temp, dist, weight)
    }

    //println(s"$i")
    //i = i + 1

    tempsWithDistances.filter{ case (p, temp, dist, weight) => dist <= toleranceDistance }
                .headOption match {
      case Some((p, temp, dist, w)) => temp
      case None => tempsWithDistances.map{ case (p, temp, dist, weight) => weight * temp }.sum /
                   tempsWithDistances.map{ case (p, temp, dist, weight) => weight }.sum
    }
  }

  /**
   * The distance between two points in [km]
   */
  def distance(p1: Location, p2: Location): Double = {
    def antipodes(p1: Location, p2: Location) = 
      p1.lat == -p2.lat && abs(p1.lon - p2.lon) == 180.0D

    def centralAngle(p1: Location, p2: Location) =
      if (p1 == p2) 0.0D
      else if (antipodes(p1, p2)) math.Pi
      else {
        val p1Lat = toRadians(p1.lat)
        val p2Lat = toRadians(p2.lat)
        val deltaLat = abs(p1Lat - p2Lat)
        val deltaLon = toRadians(abs(p1.lon - p2.lon))
        2 * asin(sqrt(pow(sin(deltaLat / 2), 2) + cos(p1Lat) * cos(p2Lat) * pow(sin(deltaLon / 2), 2)))
      }

    val earthRadius = 6371.0D // km

    earthRadius * centralAngle(p1, p2)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val (exact, notExact) = points.partition{ case (temp, color) => temp == value }

    if (!exact.isEmpty) exact.head._2
    else {
      val (lowerPoints, upperPoints) = notExact.partition{ case (temp, color) => temp < value }

      if (lowerPoints.isEmpty) points.minBy(_._1)._2
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
  }

  

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180
    
    def indexToLocation(idx: Int) = {
      val lon = idx % width - width / 2.0D
      val lat = -(idx / width - height / 2.0D)
      Location(lat, lon)
    }

    val pixels = (0 until (width * height)).par
      .map(indexToLocation(_))
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map{ case Color(r, g, b) => Pixel(r, g, b, 255) }
      .toArray

    Image(width, height, pixels) 
  }
}


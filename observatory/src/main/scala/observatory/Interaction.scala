package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{toDegrees, pow, atan, sinh, Pi}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  import Visualization._

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val lat = toDegrees(atan(sinh(Pi - (tile.y * 2 * Pi) / pow(2, tile.zoom))))
    val lon = (tile.x * 360) / pow(2, tile.zoom) - 180
    Location(lat, lon)
  }

  val zoomOffset = 8
  val width = pow(2, zoomOffset).toInt
  val height = width

  def tilePixelLocations(tile: Tile): Iterable[Location] = {
    

    def offset(c: Int) = (c * pow(2, zoomOffset)).toInt
    def zoomedTile(x: Int, y: Int) = Tile(x + offset(tile.x), y + offset(tile.y), tile.zoom + zoomOffset)

    val coordinates = for {
      y <- 0 until height
      x <- 0 until width
    } yield (x, y)

    coordinates.map{ case (x, y) => zoomedTile(x, y) }
      .map(tileLocation)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val alpha = 127

    val pixels = tilePixelLocations(tile).par
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map{ case Color(r, g, b) => Pixel(r, g, b, alpha) }
      .toArray

    Image(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
      (year, data) <- yearlyData
    } generateImage(year, Tile(x, y, zoom), data)
  }
}

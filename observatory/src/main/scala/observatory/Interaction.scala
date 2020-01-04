package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{toDegrees, atan, sinh, Pi}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * tile.y.toDouble / (1 << tile.zoom))))), 
      tile.x.toDouble / (1 << tile.zoom) * 360.0 - 180.0,
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    def computeLocations(tile: Tile, zoomCount: Int): List[Location] = {
      if (zoomCount == 0) List(tileLocation(tile))
      else {
        computeLocations(Tile(2 * tile.x, 2 * tile.y, tile.zoom * 2), zoomCount - 1) ++
        computeLocations(Tile(2 * tile.x + 1, 2 * tile.y, tile.zoom * 2), zoomCount - 1) ++
        computeLocations(Tile(2 * tile.x, 2 * tile.y + 1, tile.zoom * 2), zoomCount - 1) ++
        computeLocations(Tile(2 * tile.x + 1, 2 * tile.y + 1, tile.zoom * 2), zoomCount - 1)
      }
    }
    val pixels = computeLocations(tile, 8)
      .sortBy{ case Location(lat, lon) => lon * 1000 + lat }
      .par
      .map{ location => 
        val color = Visualization.interpolateColor(colors, 
          Visualization.predictTemperature(temperatures, location))
        println(s"computeColor: $color")
        Pixel(color.red, color.green, color.blue, 127)
      }
    println(pixels.size)
    ???

      //
      //}.toArray


    //Image(256, 256, pixels) 
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

    ???
  }

}

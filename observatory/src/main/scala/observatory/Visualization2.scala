package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{sqrt, pow, ceil, floor, abs}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {
  import Interaction._
  import Visualization._

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) + 
    d10 * point.x * (1 - point.y) + 
    d01 * point.y * (1 - point.x) + 
    d11 * point.x * point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    def interpolateWithGrid(location: Location): Temperature = {
      val lat = if (location.lat >= 0) ceil(location.lat).toInt else location.lat.toInt
      val lon = if (location.lon >= 0) location.lon.toInt else floor(location.lon).toInt

      val point = CellPoint(abs(location.lon - lon), abs(location.lat - lat))
      val d00 = grid(GridLocation(lat, lon))
      val d01 = grid(GridLocation(lat - 1, lon))
      val d10 = grid(GridLocation(lat, lon + 1))
      val d11 = grid(GridLocation(lat - 1, lon + 1))

      bilinearInterpolation(point, d00, d01, d10, d11)
    }

    val pixels = tilePixelLocations(tile).par
      .map(interpolateWithGrid)
      .map(interpolateColor(colors, _))
      .map{ case Color(r, g, b) => Pixel(r, g, b, 127) }
      .toArray

    val width = sqrt(pixels.size).toInt
    val height = width

    Image(width, height, pixels)
  }

}

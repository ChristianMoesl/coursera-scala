package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {
  import Visualization._

  val gridLocations = for {
      lat <- -89 to 90
      lon <- -180 to 179
    } yield GridLocation(lat, lon)

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val predictions = gridLocations.par
      .map(g => (g, predictTemperature(temperatures, Location(g.lat, g.lon))))
      .toMap
    
    predictions.apply
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val grids = temperaturess.par map makeGrid
    val numberOfGrids = grids.size.toDouble

    def averageLocations(location: GridLocation): (GridLocation, Temperature) = 
      (location, (grids map (grid => grid(location)) sum) / numberOfGrids)
    
    gridLocations.par.map(averageLocations).toMap.apply
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = makeGrid(temperatures)

    def deviationPerLocation(location: GridLocation): (GridLocation, Temperature) =
      (location, grid(location) - normals(location))

    gridLocations.par.map(deviationPerLocation).toMap.apply
  }
}


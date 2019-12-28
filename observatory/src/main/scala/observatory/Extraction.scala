package observatory

import java.time.LocalDate
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  type ID = (Option[Int], Option[Int])

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stations = readStations(stationsFile)

    readTemperatures(temperaturesFile, year)
      .filter(r => stations.contains(r._1))
      .map{ case (id, TemperatureRecord(date, temp)) => (date, stations(id), temp) }
      .toIterable
  }

  def fahrenheitToCelsius(value: Temperature): Temperature = (value - 32.0D) / 1.8D

  def readFile(file: String): Iterator[String] =
    Source.fromInputStream(getClass.getResourceAsStream(file), "utf-8")
          .getLines

  def parseInt(s: String): Option[Int] = if (s == "") None else Some(s.toInt)

  def readStations(file: String): Map[ID, Location] = {
    def parseLocation(lon: String, lat: String): Option[Location] = 
      if (lon != "" && lat != "") Some(Location(lat.toDouble, lon.toDouble)) else None

    readFile(file).map(_.split(","))
      .filter(_.length == 4)
      .map(xs => ((parseInt(xs(0)), parseInt(xs(1))), parseLocation(xs(3), xs(2))))
      .filter(_._2.isDefined)
      .map{ case (k, v) => (k, v.get) }
      .toMap
  }

  case class TemperatureRecord(date: LocalDate, temp: Temperature)
  
  def readTemperatures(file: String, year: Year): Iterator[(ID, TemperatureRecord)] = {
    readFile(file).map(_.split(","))
      .filter(_.length == 5)
      .map(xs => (
        (parseInt(xs(0)), parseInt(xs(1))), 
        TemperatureRecord(
          LocalDate.of(year, xs(2).toInt, xs(3).toInt),
          fahrenheitToCelsius(xs(4).toDouble)
        )
      ))
      .filter(_._2.temp <= 9999.9D)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.map(x => (x._2, x._3))
      .groupBy(_._1)
      .mapValues(xs => xs.aggregate(0.0D)((r, x) => r + x._2, (_ + _)) / xs.size)
  }

}

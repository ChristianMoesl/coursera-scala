package observatory

import org.junit.Assert._
import org.junit.Test
import java.time.LocalDate
import org.scalameter.measure

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  import Extraction._

  // Implement tests for the methods of the `Extraction` object
  //lazy val records = locateTemperatures(1975, "/stations.csv", "/1975.csv")

  //@Test def `merge temperatures and stations`() = {
    //println(measure{
      //assert(records.toList.contains(
        //(LocalDate.of(1975, 1, 1), Location(70.933D, -8.667), -4.888888888888889D)
      //))
    //})
  //}

  //@Test def `compute average temperatures`() = {
    //println(measure {
      //val averages = locationYearlyAverageRecords(records)
      //assert(averages.toList.contains((Location(67.55,-63.783),-6.654451137884884D)))
    //})
  //}
}

package observatory

import scala.collection.concurrent.TrieMap
import org.junit.Assert._
import org.junit.Test
import org.scalatest._
import matchers.should.Matchers._

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  import Interaction._
  import Visualization._
  import Extraction._

  val temperatureColors = List(
    (60.0D,Color(255,255,255)),
    (32.0D,Color(255,0,0)),
    (12.0D,Color(255,255,0)),
    (0.0D, Color(0,255,255)),
    (-15.0D,Color(0,0,255)),
    (-27.0D,Color(255,0,255)),
    (-50.0D,Color(33,0,107)),
    (-60.0D,Color(0,0,0))
  )


  //@Test def `make image from tile`(): Unit = {
    //val records = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    //val averages = Extraction.locationYearlyAverageRecords(records)

    //val image = tile(averages, temperatureColors, Tile(0, 0, 1))

    //image.output(new java.io.File("target/some-image.png"))

    //println(averages.size)
  //}
  //

  //@Test def `generate tiles for test example`(): Unit = {
    //val records = locateTemperatures(2015, "/stations.csv", "/2015.csv")
    //val averages = locationYearlyAverageRecords(records)
  
    //def generateImage(year: Int, t: Tile, data: Iterable[(Location, Temperature)]): Unit = {
      //val image = tile(data, temperatureColors, t)
      //image.output(new java.io.File(s"target/temperatures/${year}/${t.zoom}/${t.x}-${t.y}.png"))
      //println(s"generated tile: $t")
    //}

    //generateTiles(List((2015, averages)), generateImage)
  //}
}

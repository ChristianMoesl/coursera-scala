package observatory

import org.junit.Assert._
import org.junit.Test
import org.scalatest._
import Matchers._

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  import Visualization._
  import Extraction._

  // Implement tests for the methods of the `Visualization` object

  @Test def `interpolate colors`() = {
    val points = List((10.0D, Color(255, 255, 255)), (20.0D, Color(0, 0, 0)), (30.0D, Color(255, 0, 0)))

    assertEquals(Color(128, 128, 128), interpolateColor(points, 15.0D))
    assertEquals(Color(128, 0, 0), interpolateColor(points, 25.0D))
  }

  @Test def `interpolate more colors`(): Unit = {
    val points = List((0.0,Color(255,0,0)), (1.0,Color(0,0,255)))

    interpolateColor(points, 0.25) should equal (Color(191,0,64))
    interpolateColor(points, 0.5) should equal (Color(128,0,128))
  }

  @Test def `predicted temperature at location z should be close to close location x`(): Unit = {
    val points = List(
      (Location(90.0, 90.0), 10.0D),
      (Location(90.0, 180.0), 20.0D)
    )

    predictTemperature(points, Location(100.0, 180.0)) should be >= 15.0D
    predictTemperature(points, Location(90.0, 100.0)) should be <= 15.0D
  }

  val vienna = Location(48.205715D, 16.372489D)
  val london = Location(51.507034D, -0.127862D)
  val viennaAntipode = Location(-vienna.lat, vienna.lon + 180.0D)

  @Test def `distance between two known points on earth`(): Unit = {
    distance(vienna, london) should be >= 1200.0D
    distance(vienna, london) should be <= 1350.0D
  }

  @Test def `distance between two antipodes`(): Unit = {
    val earthRadius = 6700.0D
    distance(vienna, viennaAntipode) should be >= (2 * earthRadius)
    distance(vienna, viennaAntipode) should be < (6 * earthRadius)
  }

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
  
  @Test def `visualize image`(): Unit = {
    val records = locateTemperatures(1975, "/stations.csv", "/1975.csv")
    val averages = locationYearlyAverageRecords(records)

    val image = visualize(averages, temperatureColors)

    image.output(new java.io.File("target/some-image.png"))
    
    println(averages.size)

  }
}

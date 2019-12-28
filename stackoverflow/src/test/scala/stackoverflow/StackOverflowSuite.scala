package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.junit._
import org.junit.Assert.assertEquals
import java.io.File

object StackOverflowSuite {
  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")
  val sc: SparkContext = new SparkContext(conf)
}

class StackOverflowSuite {
  import StackOverflowSuite._


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  lazy val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
  lazy val raw     = testObject.rawPostings(lines)
  lazy val grouped = testObject.groupedPostings(raw)
  lazy val scored  = testObject.scoredPostings(grouped).cache()
  lazy val vectored = testObject.vectorPostings(scored)


  @Test def `testObject can be instantiated`: Unit = {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }
  
  @Test def `scored postings are at least of size 2121822`: Unit = {
    val s = scored.collect()

    assert(s.size == 2121822)

    assert(s.contains((Posting(1, 6, None, None, 140, Some("CSS")), 67)))
    assert(s.contains((Posting(1, 42,  None, None, 155, Some("PHP")),  89)))
    assert(s.contains((Posting(1, 72,  None, None, 16,  Some("Ruby")), 3)))
    assert(s.contains((Posting(1, 126, None, None, 33,  Some("Java")), 30)))
    assert(s.contains((Posting(1, 174, None, None, 38,  Some("C#")),   20)))
  }

  @Test def `vectoring of languages works`: Unit = {
    val v = vectored.collect.toList

    assert(v.contains((350000, 67)))
    assert(v.contains((100000, 89)))
    assert(v.contains((300000, 3)))
    assert(v.contains((50000,  30)))
    assert(v.contains((200000, 20)))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(100 * 1000)
}

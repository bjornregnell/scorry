package scorry

import com.raquo.laminar.api.L.*
import com.raquo.laminar.api.L.svg as s
import com.raquo.laminar.codecs.StringAsIsCodec
import org.scalajs.dom

object Main:

  def mean(values: Seq[Double]): Double =
    values.sum / values.length

  def geometricMean(values: Seq[Double]): Double =
    math.pow(values.product, 1.0 / values.length)

  def median(values: Seq[Double]): Double =
    val sorted = values.sorted
    val n = sorted.length
    if n % 2 == 1 then sorted(n / 2)
    else (sorted(n / 2 - 1) + sorted(n / 2)) / 2.0

  def mode(values: Seq[Double]): Seq[Double] =
    val counts = values.groupBy(identity).view.mapValues(_.length)
    val maxCount = counts.values.max
    if maxCount == 1 then Seq.empty
    else counts.collect { case (v, c) if c == maxCount => v }.toSeq.sorted

  def variance(values: Seq[Double]): Double =
    val m = mean(values)
    values.map(x => (x - m) * (x - m)).sum / values.length

  def stddev(values: Seq[Double]): Double =
    math.sqrt(variance(values))

  def range(values: Seq[Double]): Double =
    values.max - values.min

  def quartile(sorted: Seq[Double], q: Double): Double =
    val pos = q * (sorted.length - 1)
    val lo = pos.toInt
    val hi = math.min(lo + 1, sorted.length - 1)
    val frac = pos - lo
    sorted(lo) * (1 - frac) + sorted(hi) * frac

  private def attr(name: String) = s.svgAttr(name, StringAsIsCodec, None)

  def boxPlot(values: Seq[Double]): SvgElement =
    val sorted = values.sorted
    val minVal = sorted.head
    val q1     = quartile(sorted, 0.25)
    val med    = quartile(sorted, 0.5)
    val q3     = quartile(sorted, 0.75)
    val maxVal = sorted.last

    val w = 100.0; val h = 280.0; val pad = 30.0
    val plotH = h - 2 * pad; val cx = w / 2; val boxW = 30.0

    // y is inverted: max value at top, min at bottom
    def y(v: Double): Double =
      if maxVal == minVal then pad + plotH / 2
      else pad + (1 - (v - minVal) / (maxVal - minVal)) * plotH

    def fmt(v: Double): String = f"$v%.1f"

    def ln(xa: Double, ya: Double, xb: Double, yb: Double, color: String = "#333", sw: String = "1") =
      s.line(attr("x1") := s"$xa", attr("y1") := s"$ya",
        attr("x2") := s"$xb", attr("y2") := s"$yb",
        attr("stroke") := color, attr("stroke-width") := sw)

    def lbl(yp: Double, txt: String) =
      s.text(attr("x") := s"${cx + boxW/2 + 8}", attr("y") := s"${yp + 4}",
        attr("text-anchor") := "start", attr("font-size") := "11", txt)

    s.svg(
      attr("width") := s"${w.toInt}", attr("height") := s"${h.toInt}",
      // whisker line
      ln(cx, y(minVal), cx, y(maxVal)),
      // min cap
      ln(cx - boxW/2, y(minVal), cx + boxW/2, y(minVal)),
      // max cap
      ln(cx - boxW/2, y(maxVal), cx + boxW/2, y(maxVal)),
      // box Q1–Q3
      s.rect(attr("x") := s"${cx - boxW/2}", attr("y") := s"${y(q3)}",
        attr("width") := s"$boxW", attr("height") := s"${y(q1) - y(q3)}",
        attr("fill") := "#6ca0dc", attr("stroke") := "#333", attr("stroke-width") := "1"),
      // median line
      ln(cx - boxW/2, y(med), cx + boxW/2, y(med), "#c00", "2"),
      // labels
      lbl(y(maxVal), fmt(maxVal)), lbl(y(q3), fmt(q3)), lbl(y(med), fmt(med)),
      lbl(y(q1), fmt(q1)), lbl(y(minVal), fmt(minVal)),
    )

  def parseDoubles(input: String): Seq[Double] =
    input.trim.split("\\s+").filter(_.nonEmpty).map(_.toDouble)

  def parsePairs(input: String): Seq[(Double, Double)] =
    input.split(";").map(_.trim).filter(_.nonEmpty).map: pair =>
      val nums = pair.split("\\s+").map(_.toDouble)
      (nums(0), nums(1))

  def pearsonRankOrderCorrelation(pairs: Seq[(Double, Double)]): Double =
    val n = pairs.length
    val xs = pairs.map(_._1)
    val ys = pairs.map(_._2)
    val meanX = mean(xs)
    val meanY = mean(ys)
    val num = pairs.map((x, y) => (x - meanX) * (y - meanY)).sum
    val denX = math.sqrt(xs.map(x => (x - meanX) * (x - meanX)).sum)
    val denY = math.sqrt(ys.map(y => (y - meanY) * (y - meanY)).sum)
    num / (denX * denY)

  def main(args: Array[String]): Unit =
    val inputText = Var("")
    val numsSignal = inputText.signal.map: text =>
      try
        val nums = parseDoubles(text)
        if nums.isEmpty then None else Some(nums)
      catch
        case _: NumberFormatException => None

    val statsSignal = numsSignal.map:
      case None => Seq.empty
      case Some(nums) =>
        val modeVals = mode(nums)
        val modeStr = if modeVals.isEmpty then "no mode" else modeVals.mkString(", ")
        Seq(
          s"Arithmetic Mean: ${mean(nums)}",
          s"Geometric Mean: ${geometricMean(nums)}",
          s"Median: ${median(nums)}",
          s"Mode: $modeStr",
          s"Variance: ${variance(nums)}",
          s"Std Dev: ${stddev(nums)}",
          s"Range: ${range(nums)}"
        )

    val corrInput  = Var("")
    val corrResult = Var("")

    val app = div(
      label("Enter numbers separated by spaces"),
      br(),
      input(
        typ         := "text",
        placeholder := "1.5 3.7 2.0 8.3 4.1",
        onInput.mapToValue --> inputText
      ),
      div(
        display := "flex",
        gap     := "2em",
        alignItems := "flex-start",
        div(children <-- statsSignal.map(lines => lines.flatMap(line => Seq(br(), span(line))))),
        child.maybe <-- numsSignal.map(_.filter(_.length >= 2).map(boxPlot))
      ),
      hr(),
      label("Enter pairs of numbers (pairs separated by semicolons)"),
      br(),
      input(
        typ         := "text",
        placeholder := "1 2 ; 3 4 ; 5 6 ; 7 8",
        onInput.mapToValue --> corrInput
      ),
      br(),
      button(
        "Calculate Correlation",
        onClick --> { _ =>
          corrResult.set(
            try
              val pairs = parsePairs(corrInput.now())
              if pairs.length < 2 then "Need at least 2 pairs"
              else s"Pearson r: ${pearsonRankOrderCorrelation(pairs)}"
            catch
              case _: Exception => "Invalid input"
          )
        }
      ),
      span(" "),
      child.text <-- corrResult
    )

    renderOnDomContentLoaded(dom.document.getElementById("app"), app)

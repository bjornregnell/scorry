package scorry

import com.raquo.laminar.api.L.*
import com.raquo.laminar.api.L.svg as s
import com.raquo.laminar.codecs.StringAsIsCodec

object Plots:

  private def attr(name: String) = s.svgAttr(name, StringAsIsCodec, None)

  def boxPlot(values: Seq[Double]): SvgElement =
    val sorted = values.sorted
    val minVal = sorted.head
    val q1     = SummaryStatistics.quartile(sorted, 0.25)
    val med    = SummaryStatistics.quartile(sorted, 0.5)
    val q3     = SummaryStatistics.quartile(sorted, 0.75)
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

  def scatterPlot(pairs: Seq[(Double, Double)]): SvgElement =
    val xs = pairs.map(_._1)
    val ys = pairs.map(_._2)
    val xMin = xs.min; val xMax = xs.max
    val yMin = ys.min; val yMax = ys.max

    val w = 280.0; val h = 280.0; val pad = 40.0
    val plotW = w - 2 * pad; val plotH = h - 2 * pad

    def px(v: Double): Double =
      if xMax == xMin then pad + plotW / 2
      else pad + (v - xMin) / (xMax - xMin) * plotW

    def py(v: Double): Double =
      if yMax == yMin then pad + plotH / 2
      else pad + (1 - (v - yMin) / (yMax - yMin)) * plotH

    def fmt(v: Double): String = f"$v%.1f"

    def ln(xa: Double, ya: Double, xb: Double, yb: Double) =
      s.line(attr("x1") := s"$xa", attr("y1") := s"$ya",
        attr("x2") := s"$xb", attr("y2") := s"$yb",
        attr("stroke") := "#ccc", attr("stroke-width") := "1")

    val dots = pairs.map: (x, y) =>
      s.circle(attr("cx") := s"${px(x)}", attr("cy") := s"${py(y)}",
        attr("r") := "4", attr("fill") := "#336699")

    val xTicks = Seq(xMin, (xMin + xMax) / 2, xMax).map: v =>
      s.text(attr("x") := s"${px(v)}", attr("y") := s"${pad + plotH + 18}",
        attr("text-anchor") := "middle", attr("font-size") := "11", fmt(v))

    val yTicks = Seq(yMin, (yMin + yMax) / 2, yMax).map: v =>
      s.text(attr("x") := s"${pad - 8}", attr("y") := s"${py(v) + 4}",
        attr("text-anchor") := "end", attr("font-size") := "11", fmt(v))

    val xLabel = s.text(attr("x") := s"${pad + plotW / 2}", attr("y") := s"${h - 2}",
      attr("text-anchor") := "middle", attr("font-size") := "12", "x")
    val yLabel = s.text(attr("x") := "4", attr("y") := s"${pad + plotH / 2}",
      attr("text-anchor") := "middle", attr("font-size") := "12",
      attr("transform") := s"rotate(-90, 4, ${pad + plotH / 2})", "y")

    s.svg(
      attr("width") := s"${w.toInt}", attr("height") := s"${h.toInt}",
      // axes
      ln(pad, pad, pad, pad + plotH),
      ln(pad, pad + plotH, pad + plotW, pad + plotH),
      // dots, ticks, labels
      s.g(dots*),
      s.g(xTicks*),
      s.g(yTicks*),
      xLabel, yLabel,
    )

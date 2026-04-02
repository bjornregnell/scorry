package scorry

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import SummaryStatistics.*
import Plots.*

object Main:

  def main(args: Array[String]): Unit =
    val inputText = Var("1.2 3.5 5.8 9.1 9.1 10")
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
          (s"Arithmetic Mean: ${mean(nums)}", "Sum of all values divided by the number of values"),
          (s"Geometric Mean: ${geometricMean(nums)}", "The nth root of the product of n values; useful for growth rates"),
          (s"Median: ${median(nums)}", "The middle value when sorted; less sensitive to outliers than the mean"),
          (s"Mode: $modeStr", "The most frequently occurring value(s); 'no mode' if all values are unique"),
          (s"Variance: ${variance(nums)}", "Average of squared deviations from the mean; measures spread"),
          (s"Std Dev: ${stddev(nums)}", "Square root of variance; measures spread in the same unit as the data"),
          (s"Range: ${range(nums)}", "Difference between the maximum and minimum values")
        )

    val corrInput = Var("1 2 ; 3 4 ; 5 6 ; 7 8")
    val corrSignal = corrInput.signal.map: text =>
      try
        val pairs = parsePairs(text)
        if pairs.length < 2 then Seq.empty
        else Seq(
          (s"Pearson r: ${pearsonCorrelation(pairs)}",
            "Pearson product-moment correlation coefficient: measures linear association between two variables. " +
            "Ranges from -1 (perfect negative) to +1 (perfect positive). " +
            "Assumes: (1) both variables are continuous, (2) the relationship is linear, " +
            "(3) no significant outliers, (4) variables are approximately normally distributed."),
          (s"Spearman rho: ${spearmanCorrelation(pairs)}",
            "Spearman rank correlation: measures monotonic association using ranks instead of raw values. " +
            "Ranges from -1 to +1. More robust to outliers than Pearson. " +
            "Assumes: (1) both variables are at least ordinal, (2) the relationship is monotonic.")
        )
      catch
        case _: Exception => Seq(("Invalid input", ""))

    val app = div(
      label("Enter numbers separated by spaces"),
      br(),
      input(
        typ   := "text",
        value := inputText.now(),
        onInput.mapToValue --> inputText
      ),
      div(
        display := "flex",
        gap     := "2em",
        alignItems := "flex-start",
        div(children <-- statsSignal.map(items => items.flatMap((text, tip) =>
          Seq(br(), span(cls := "tip", dataAttr("tip") := tip, text))))),
        child.maybe <-- numsSignal.map(_.filter(_.length >= 2).map(boxPlot))
      ),
      hr(),
      label("Enter pairs of numbers (pairs separated by semicolons)"),
      br(),
      input(
        typ   := "text",
        value := corrInput.now(),
        onInput.mapToValue --> corrInput
      ),
      br(),
      div(children <-- corrSignal.map(items => items.flatMap((text, tip) =>
        Seq(br(), span(cls := "tip", dataAttr("tip") := tip, text)))))
    )

    renderOnDomContentLoaded(dom.document.getElementById("app"), app)

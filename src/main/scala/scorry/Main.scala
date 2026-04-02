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
          (s"N: ${nums.length}", "The number of observations in the data set"),
          (s"Arithmetic Mean: ${mean(nums)}", "Sum of all values divided by the number of values"),
          (s"Geometric Mean: ${geometricMean(nums)}", "The nth root of the product of n values; useful for growth rates"),
          (s"Median: ${median(nums)}", "The middle value when sorted; less sensitive to outliers than the mean"),
          (s"Mode: $modeStr", "The most frequently occurring value(s); 'no mode' if all values are unique"),
          (s"Variance: ${variance(nums)}", "Average of squared deviations from the mean; measures spread"),
          (s"Std Dev: ${stddev(nums)}", "Square root of variance; measures spread in the same unit as the data"),
          (s"Range: ${range(nums)}", "Difference between the maximum and minimum values")
        )

    val corrInput = Var("1 1 ; 2 3 ; 3 6 ; 4 10 ; 5 15 ; 10 100")
    val corrSignal = corrInput.signal.map: text =>
      try
        val pairs = parsePairs(text)
        if pairs.length < 2 then Seq.empty
        else Seq(
          (s"N: ${pairs.length} pairs", "The number of paired observations"),
          (s"Spearman rho: ${spearmanCorrelation(pairs)}",
            "Spearman rank correlation: measures monotonic association using ranks instead of raw values. " +
            "Ranges from -1 to +1. More robust to outliers than Pearson. " +
            "Assumes: (1) both variables are at least ordinal, (2) the relationship is monotonic."),
          { val r = pearsonCorrelation(pairs); val p = pearsonPValue(r, pairs.length)
          (s"Pearson r: $r, p = ${f"$p%.4f"}",
            "Pearson product-moment correlation coefficient: measures linear association between two variables. " +
            "Ranges from -1 (perfect negative) to +1 (perfect positive). " +
            "Assumes: (1) both variables are continuous, (2) the relationship is linear, " +
            "(3) no significant outliers, (4) variables are approximately normally distributed.") },
          { val (t, df, p) = pairedTTest(pairs)
            (s"Paired t-test: t = ${f"$t%.4f"}, df = $df, p = ${f"$p%.4f"}",
              "Student's paired t-test: tests whether the mean difference between paired observations is zero. " +
              "The p-value is the probability of observing a t-statistic this extreme if the true mean difference is zero (two-tailed). " +
              "p < 0.05 is conventionally considered statistically significant, but consider effect size and sample size too. " +
              "p < 0.01 is highly significant; p > 0.1 suggests no evidence against equal means. " +
              "Related to Pearson r: both assume normality and continuous data. " +
              "A significant Pearson r means the variables co-vary, while a significant t-test means their means differ. " +
              "They are complementary — high correlation does not imply equal means, and equal means does not imply no correlation. " +
              "Assumes: (1) differences are normally distributed, (2) pairs are independent, (3) data is continuous.") }
        )
      catch
        case _: Exception => Seq(("Invalid input", ""))

    val pairsSignal = corrInput.signal.map: text =>
      try
        val pairs = parsePairs(text)
        if pairs.length < 2 then None else Some(pairs)
      catch
        case _: Exception => None

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
      div(
        display := "flex",
        gap     := "2em",
        alignItems := "flex-start",
        div(children <-- corrSignal.map(items => items.flatMap((text, tip) =>
          Seq(br(), span(cls := "tip", dataAttr("tip") := tip, text))))),
        child.maybe <-- pairsSignal.map(_.map(scatterPlot))
      )
    )

    renderOnDomContentLoaded(dom.document.getElementById("app"), app)

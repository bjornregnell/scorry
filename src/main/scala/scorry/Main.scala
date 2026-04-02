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
          ("N", s"${nums.length}", "The number of observations in the data set",
            "https://en.wikipedia.org/wiki/Sample_size_determination"),
          ("Arithmetic Mean", s"${mean(nums)}", "Sum of all values divided by the number of values",
            "https://en.wikipedia.org/wiki/Arithmetic_mean"),
          ("Geometric Mean", s"${geometricMean(nums)}", "The nth root of the product of n values; useful for growth rates",
            "https://en.wikipedia.org/wiki/Geometric_mean"),
          ("Median", s"${median(nums)}", "The middle value when sorted; less sensitive to outliers than the mean",
            "https://en.wikipedia.org/wiki/Median"),
          ("Mode", s"$modeStr", "The most frequently occurring value(s); 'no mode' if all values are unique",
            "https://en.wikipedia.org/wiki/Mode_(statistics)"),
          ("Variance", s"${variance(nums)}", "Average of squared deviations from the mean; measures spread",
            "https://en.wikipedia.org/wiki/Variance"),
          ("Std Dev", s"${stddev(nums)}", "Square root of variance; measures spread in the same unit as the data",
            "https://en.wikipedia.org/wiki/Standard_deviation"),
          ("Range", s"${range(nums)}", "Difference between the maximum and minimum values",
            "https://en.wikipedia.org/wiki/Range_(statistics)")
        )

    val corrInput = Var("1 2.1 ; 2.5 5 ; 3 4.3 ; 4.2 9 ; 5 7.5 ; 6.1 12 ; 7 10.4 ; 8.3 15")
    val corrSignal = corrInput.signal.map: text =>
      try
        val pairs = parsePairs(text)
        if pairs.length < 2 then Seq.empty
        else Seq(
          ("N", s"${pairs.length} pairs", "The number of paired observations",
            "https://en.wikipedia.org/wiki/Sample_size_determination"),
          { val rho = spearmanCorrelation(pairs); val p = pearsonPValue(rho, pairs.length)
          ("Spearman rho", s"$rho, p = ${f"$p%.4f"}",
            "Spearman rank correlation: measures monotonic association using ranks instead of raw values. " +
            "Ranges from -1 to +1. More robust to outliers than Pearson. " +
            "The p-value tests the null hypothesis that there is no monotonic association (rho = 0). " +
            "p < 0.05 is conventionally considered statistically significant; p < 0.01 is highly significant; " +
            "p > 0.1 suggests no evidence of monotonic association. " +
            "The p-value is computed using the t-distribution approximation: t = rho * sqrt((n-2)/(1-rho²)), " +
            "which is accurate for n >= 10. " +
            "Assumes: (1) both variables are at least ordinal, (2) the relationship is monotonic.",
            "https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient") },
          { val r = pearsonCorrelation(pairs); val p = pearsonPValue(r, pairs.length)
          ("Pearson r", s"$r, p = ${f"$p%.4f"}",
            "Pearson product-moment correlation coefficient: measures linear association between two variables. " +
            "Ranges from -1 (perfect negative) to +1 (perfect positive). " +
            "Assumes: (1) both variables are continuous, (2) the relationship is linear, " +
            "(3) no significant outliers, (4) variables are approximately normally distributed.",
            "https://en.wikipedia.org/wiki/Pearson_correlation_coefficient") },
          { val (t, df, p) = pairedTTest(pairs)
            ("Paired t-test", s"t = ${f"$t%.4f"}, df = $df, p = ${f"$p%.4f"}",
              "Student's paired t-test: tests whether the mean difference between paired observations is zero. " +
              "The p-value is the probability of observing a t-statistic this extreme if the true mean difference is zero (two-tailed). " +
              "p < 0.05 is conventionally considered statistically significant, but consider effect size and sample size too. " +
              "p < 0.01 is highly significant; p > 0.1 suggests no evidence against equal means. " +
              "Related to Pearson r: both assume normality and continuous data. " +
              "A significant Pearson r means the variables co-vary, while a significant t-test means their means differ. " +
              "They are complementary — high correlation does not imply equal means, and equal means does not imply no correlation. " +
              "Assumes: (1) differences are normally distributed, (2) pairs are independent, (3) data is continuous.",
              "https://en.wikipedia.org/wiki/Student%27s_t-test#Paired_samples") }
        )
      catch
        case _: Exception => Seq(("Invalid input", "", "", ""))

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
        flexWrap := "wrap",
        gap     := "2em",
        alignItems := "flex-start",
        overflowX := "auto",
        div(children <-- statsSignal.map(items => items.flatMap((lbl, val_, tip, url) =>
          Seq(br(), span(cls := "tip", dataAttr("tip") := tip,
            a(lbl, href := url, target := "_blank"), s": $val_"))))),
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
        flexWrap := "wrap",
        gap     := "2em",
        alignItems := "flex-start",
        overflowX := "auto",
        div(children <-- corrSignal.map(items => items.flatMap((lbl, val_, tip, url) =>
          Seq(br(), span(cls := "tip", dataAttr("tip") := tip,
            a(lbl, href := url, target := "_blank"), s": $val_"))))),
        child.maybe <-- pairsSignal.map(_.map(scatterPlot))
      )
    )

    renderOnDomContentLoaded(dom.document.getElementById("app"), app)

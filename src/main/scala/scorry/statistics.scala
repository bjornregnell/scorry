package scorry

object SummaryStatistics:

  /** Computes the arithmetic mean (average) of a sequence of values.
    *
    * Defined as the sum of all values divided by the number of values:
    * mean = (1/n) * Σxᵢ
    *
    * @param values a non-empty sequence of numeric values
    * @return the arithmetic mean
    */
  def mean(values: Seq[Double]): Double =
    values.sum / values.length

  /** Computes the geometric mean of a sequence of positive values.
    *
    * Defined as the nth root of the product of n values:
    * geometricMean = (∏xᵢ)^(1/n)
    *
    * Useful for averaging ratios, growth rates, and quantities that
    * multiply together. Unlike the arithmetic mean, it is not dominated
    * by large outliers and gives a meaningful "typical" multiplicative factor.
    * All values should be positive; negative or zero values produce NaN.
    *
    * @param values a non-empty sequence of positive numeric values
    * @return the geometric mean
    */
  def geometricMean(values: Seq[Double]): Double =
    math.pow(values.product, 1.0 / values.length)

  /** Computes the median (middle value) of a sequence of values.
    *
    * The values are sorted and the central element is selected. For an odd
    * number of elements, the median is the middle element. For an even number,
    * it is the average of the two central elements.
    *
    * The median is a robust measure of central tendency, less sensitive
    * to outliers and skewed distributions than the arithmetic mean.
    *
    * @param values a non-empty sequence of numeric values
    * @return the median value
    */
  def median(values: Seq[Double]): Double =
    val sorted = values.sorted
    val n = sorted.length
    if n % 2 == 1 then sorted(n / 2)
    else (sorted(n / 2 - 1) + sorted(n / 2)) / 2.0

  /** Computes the mode(s) of a sequence of values.
    *
    * The mode is the value that appears most frequently. A data set may have
    * one mode (unimodal), multiple modes (multimodal), or no mode if all
    * values are unique (in which case an empty sequence is returned).
    *
    * @param values a non-empty sequence of numeric values
    * @return a sorted sequence of the most frequent value(s), or empty if all values are unique
    */
  def mode(values: Seq[Double]): Seq[Double] =
    val counts = values.groupBy(identity).view.mapValues(_.length)
    val maxCount = counts.values.max
    if maxCount == 1 then Seq.empty
    else counts.collect { case (v, c) if c == maxCount => v }.toSeq.sorted

  /** Computes the population variance of a sequence of values.
    *
    * Variance measures the average squared deviation from the mean:
    * σ² = (1/n) * Σ(xᵢ - μ)²
    *
    * This is the population variance (divides by n), not the sample variance
    * (which divides by n-1 for Bessel's correction). Use population variance
    * when your data represents the entire population.
    *
    * @param values a non-empty sequence of numeric values
    * @return the population variance
    */
  def variance(values: Seq[Double]): Double =
    val m = mean(values)
    values.map(x => (x - m) * (x - m)).sum / values.length

  /** Computes the population standard deviation of a sequence of values.
    *
    * The standard deviation is the square root of the variance, expressing
    * dispersion in the same unit as the original data:
    * σ = √(σ²)
    *
    * @param values a non-empty sequence of numeric values
    * @return the population standard deviation
    */
  def stddev(values: Seq[Double]): Double =
    math.sqrt(variance(values))

  /** Computes the range of a sequence of values.
    *
    * The range is the difference between the maximum and minimum values:
    * range = max(xᵢ) - min(xᵢ)
    *
    * It is the simplest measure of dispersion but is highly sensitive to
    * outliers since it depends only on the two most extreme values.
    *
    * @param values a non-empty sequence of numeric values
    * @return the range
    */
  def range(values: Seq[Double]): Double =
    values.max - values.min

  /** Computes a quantile of a pre-sorted sequence using linear interpolation.
    *
    * Given a quantile q in [0, 1], this method finds the position
    * pos = q * (n - 1) in the sorted array and linearly interpolates between
    * the two surrounding elements. For example, q = 0.25 gives the first
    * quartile (Q1), q = 0.5 gives the median, and q = 0.75 gives Q3.
    *
    * @param sorted a sequence of values already sorted in ascending order
    * @param q the desired quantile, between 0.0 and 1.0
    * @return the interpolated quantile value
    */
  def quartile(sorted: Seq[Double], q: Double): Double =
    val pos = q * (sorted.length - 1)
    val lo = pos.toInt
    val hi = math.min(lo + 1, sorted.length - 1)
    val frac = pos - lo
    sorted(lo) * (1 - frac) + sorted(hi) * frac

  /** Parses a whitespace-separated string of numbers into a sequence of Doubles.
    *
    * @param input a string of numbers separated by whitespace
    * @return a sequence of parsed Double values
    * @throws NumberFormatException if any token is not a valid number
    */
  def parseDoubles(input: String): Seq[Double] =
    input.trim.split("\\s+").filter(_.nonEmpty).map(_.toDouble)

  /** Parses a string of semicolon-separated pairs of numbers.
    *
    * Each pair consists of two whitespace-separated numbers. Pairs are
    * separated by semicolons. For example: "1 2 ; 3 4 ; 5 6"
    *
    * @param input a string of semicolon-separated number pairs
    * @return a sequence of (x, y) tuples
    * @throws NumberFormatException if any token is not a valid number
    * @throws ArrayIndexOutOfBoundsException if a pair has fewer than two numbers
    */
  def parsePairs(input: String): Seq[(Double, Double)] =
    input.split(";").map(_.trim).filter(_.nonEmpty).map: pair =>
      val nums = pair.split("\\s+").map(_.toDouble)
      (nums(0), nums(1))

  /** Computes the Pearson product-moment correlation coefficient for paired data.
    *
    * Measures the strength and direction of the linear relationship between
    * two variables. Defined as:
    * r = Σ((xᵢ - x̄)(yᵢ - ȳ)) / √(Σ(xᵢ - x̄)² · Σ(yᵢ - ȳ)²)
    *
    * Returns a value in [-1, +1] where -1 is perfect negative linear
    * correlation, 0 is no linear correlation, and +1 is perfect positive
    * linear correlation.
    *
    * @param pairs a sequence of (x, y) value pairs, with at least 2 pairs
    * @return the Pearson correlation coefficient
    */
  def pearsonCorrelation(pairs: Seq[(Double, Double)]): Double =
    val n = pairs.length
    val xs = pairs.map(_._1)
    val ys = pairs.map(_._2)
    val meanX = mean(xs)
    val meanY = mean(ys)
    val num = pairs.map((x, y) => (x - meanX) * (y - meanY)).sum
    val denX = math.sqrt(xs.map(x => (x - meanX) * (x - meanX)).sum)
    val denY = math.sqrt(ys.map(y => (y - meanY) * (y - meanY)).sum)
    num / (denX * denY)

  /** Assigns fractional ranks to a sequence of values, handling ties by averaging.
    *
    * The algorithm works as follows:
    *  1. Pair each value with its original index, then sort by value.
    *  2. Scan through the sorted array, grouping consecutive equal values (ties).
    *  3. For each group of ties spanning positions i to j (0-based), assign
    *     each member the average rank: (i + j) / 2 + 1 (converting to 1-based).
    *  4. Write the rank back to the original position using the stored index.
    *
    * This is the standard "average rank" tie-breaking method used in
    * non-parametric statistics (e.g. Spearman correlation, Wilcoxon test).
    *
    * @param values a sequence of numeric values
    * @return a sequence of ranks (1-based) in the same order as the input
    */
  private def ranks(values: Seq[Double]): Seq[Double] =
    val sorted = values.zipWithIndex.sortBy(_._1)
    val ranked = Array.ofDim[Double](values.length)
    var i = 0
    while i < sorted.length do
      var j = i
      while j < sorted.length - 1 && sorted(j + 1)._1 == sorted(i)._1 do j += 1
      val avgRank = (i + j) / 2.0 + 1.0
      for k <- i to j do ranked(sorted(k)._2) = avgRank
      i = j + 1
    ranked.toSeq

  /** Computes the Spearman rank correlation coefficient for paired data.
    *
    * Spearman's rho is defined as the Pearson correlation applied to the
    * ranks of the data rather than the raw values. This makes it a
    * non-parametric measure of monotonic association — it does not assume
    * linearity or normality, and is more robust to outliers than Pearson's r.
    *
    * @param pairs a sequence of (x, y) value pairs, with at least 2 pairs
    * @return the Spearman rank correlation coefficient in [-1, +1]
    */
  def spearmanCorrelation(pairs: Seq[(Double, Double)]): Double =
    val rankedPairs = ranks(pairs.map(_._1)).zip(ranks(pairs.map(_._2)))
    pearsonCorrelation(rankedPairs)

  /** Evaluates the continued fraction expansion for the regularized incomplete
    * beta function using the modified Lentz's method.
    *
    * The regularized incomplete beta function I_x(a, b) can be expressed as
    * a continued fraction:
    *   I_x(a,b) = (bt/a) · 1/(1+ d₁/(1+ d₂/(1+ ...)))
    *
    * where the coefficients dₘ alternate between two forms depending on
    * whether m is odd or even, involving a, b, and x.
    *
    * '''Lentz's method''' evaluates this continued fraction iteratively by
    * maintaining two sequences C and D that converge to the fraction's value.
    * At each step:
    *   - D_m = 1 / (1 + aₘ · D_{m-1})
    *   - C_m = 1 + aₘ / C_{m-1}
    *   - h_m = h_{m-1} · D_m · C_m
    *
    * The tiny value 1e-30 replaces exact zeros to prevent division by zero
    * (a standard safeguard in Lentz's algorithm). Convergence is declared
    * when the multiplicative correction |D·C - 1| < ε (1e-12).
    *
    * This implementation follows the algorithm in ''Numerical Recipes''
    * (Press et al.), Section 6.4.
    *
    * @param a first shape parameter of the beta distribution (> 0)
    * @param b second shape parameter (> 0)
    * @param x evaluation point in (0, 1)
    * @return the continued fraction component of I_x(a, b)
    */
  private def betacf(a: Double, b: Double, x: Double): Double =
    import scala.util.boundary, boundary.break
    val maxIter = 200; val eps = 1e-12
    val qab = a + b; val qap = a + 1; val qam = a - 1
    var c = 1.0; var d = 1.0 - qab * x / qap
    if math.abs(d) < 1e-30 then d = 1e-30
    d = 1.0 / d; var h = d
    boundary:
      for m <- 1 to maxIter do
        val m2 = 2 * m
        var aa = m.toDouble * (b - m) * x / ((qam + m2) * (a + m2))
        d = 1.0 + aa * d; if math.abs(d) < 1e-30 then d = 1e-30
        c = 1.0 + aa / c; if math.abs(c) < 1e-30 then c = 1e-30
        d = 1.0 / d; h *= d * c
        aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
        d = 1.0 + aa * d; if math.abs(d) < 1e-30 then d = 1e-30
        c = 1.0 + aa / c; if math.abs(c) < 1e-30 then c = 1e-30
        d = 1.0 / d; val del = d * c; h *= del
        if math.abs(del - 1.0) < eps then break()
    h

  /** Computes the natural logarithm of the Gamma function using the
    * Lanczos approximation.
    *
    * The Gamma function Γ(x) generalises the factorial to real (and complex)
    * numbers: Γ(n) = (n-1)! for positive integers. Computing it directly
    * overflows for moderate arguments, so we work in log-space.
    *
    * '''The Lanczos approximation''' (1964) expresses Γ(x) as:
    *   Γ(x) ≈ √(2π) · (x + g + 0.5)^(x+0.5) · e^(-(x+g+0.5)) · S(x)
    *
    * where g is a small integer (here g = 5) and S(x) is a rational series
    * with precomputed coefficients. Taking the logarithm:
    *   ln Γ(x) = 0.5·ln(2π) + (x+0.5)·ln(x+g+0.5) - (x+g+0.5) + ln(S(x))
    *
    * The six coefficients used here are from ''Numerical Recipes'' and give
    * accuracy better than 2×10⁻¹⁰ for all x > 0. We need ln Γ to compute
    * the beta function: B(a,b) = exp(lnΓ(a) + lnΓ(b) - lnΓ(a+b)).
    *
    * @param x a positive real number
    * @return ln(Γ(x))
    */
  private def lnGamma(x: Double): Double =
    val cof = Seq(76.18009172947146, -86.50532032941677, 24.01409824083091,
      -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5)
    val y = x; val tmp = x + 5.5 - (x + 0.5) * math.log(x + 5.5)
    val ser = 1.000000000190015 + cof.zipWithIndex.map((c, i) => c / (y + 1 + i)).sum
    -tmp + math.log(2.5066282746310005 * ser / x)

  /** Computes the regularized incomplete beta function I_x(a, b).
    *
    * Defined as:
    *   I_x(a, b) = B(x; a, b) / B(a, b)
    *
    * where B(x; a,b) = ∫₀ˣ t^(a-1) (1-t)^(b-1) dt is the incomplete beta
    * function and B(a,b) is the complete beta function.
    *
    * The CDF of the t-distribution (and F-distribution, binomial, etc.) can
    * be expressed in terms of I_x, making this a fundamental building block
    * for computing p-values.
    *
    * For numerical stability, the implementation uses the symmetry relation
    *   I_x(a, b) = 1 - I_{1-x}(b, a)
    * choosing whichever form converges faster based on whether
    * x < (a+1)/(a+b+2). The continued fraction `betacf` evaluates the
    * chosen form, and the front factor bt is computed in log-space via
    * `lnGamma` to avoid overflow.
    *
    * @param a first shape parameter (> 0)
    * @param b second shape parameter (> 0)
    * @param x evaluation point in [0, 1]
    * @return I_x(a, b) in [0, 1]
    */
  private def betai(a: Double, b: Double, x: Double): Double =
    if x <= 0 then 0.0
    else if x >= 1 then 1.0
    else
      val bt = math.exp(lnGamma(a + b) - lnGamma(a) - lnGamma(b) + a * math.log(x) + b * math.log(1 - x))
      if x < (a + 1) / (a + b + 2) then bt * betacf(a, b, x) / a
      else 1.0 - bt * betacf(b, a, 1 - x) / b

  /** Computes the two-tailed p-value for a t-statistic with given degrees of freedom.
    *
    * Uses the relationship between the t-distribution CDF and the regularized
    * incomplete beta function:
    *   p = I_{df/(df+t²)}(df/2, 1/2)
    *
    * This gives the two-tailed probability: the chance of observing a
    * t-statistic at least as extreme (in either direction) under the null
    * hypothesis.
    *
    * @param t the t-statistic
    * @param df degrees of freedom (> 0)
    * @return the two-tailed p-value in [0, 1]
    */
  def tTestPValue(t: Double, df: Int): Double =
    val x = df.toDouble / (df + t * t)
    betai(df / 2.0, 0.5, x)

  /** Computes the two-tailed p-value for a Pearson correlation coefficient.
    *
    * Converts the correlation coefficient r to a t-statistic using:
    *   t = r · √((n-2) / (1-r²))
    *
    * with n-2 degrees of freedom, then computes the p-value via the
    * t-distribution. This tests the null hypothesis H₀: ρ = 0 (no linear
    * correlation in the population).
    *
    * @param r the Pearson correlation coefficient in [-1, +1]
    * @param n the number of data pairs (must be > 2)
    * @return the two-tailed p-value in [0, 1]
    */
  def pearsonPValue(r: Double, n: Int): Double =
    if math.abs(r) >= 1.0 then 0.0
    else
      val t = r * math.sqrt((n - 2) / (1 - r * r))
      tTestPValue(t, n - 2)

  /** Performs Student's paired t-test on paired observations.
    *
    * Tests the null hypothesis that the mean difference between paired
    * observations is zero. Computes:
    *   - The differences dᵢ = xᵢ - yᵢ
    *   - The sample mean of differences d̄
    *   - The sample standard deviation of differences s_d (using n-1, Bessel's correction)
    *   - The t-statistic: t = d̄ / (s_d / √n)
    *   - The two-tailed p-value from the t-distribution with n-1 degrees of freedom
    *
    * @param pairs a sequence of (x, y) value pairs, with at least 2 pairs
    * @return a tuple of (t-statistic, degrees of freedom, two-tailed p-value)
    */
  def pairedTTest(pairs: Seq[(Double, Double)]): (Double, Int, Double) =
    val diffs = pairs.map((x, y) => x - y)
    val n = diffs.length
    val meanD = mean(diffs)
    val sdD = math.sqrt(diffs.map(d => (d - meanD) * (d - meanD)).sum / (n - 1))
    val t = meanD / (sdD / math.sqrt(n))
    val df = n - 1
    (t, df, tTestPValue(t, df))

  /** Computes the distance correlation (dCor) between two variables.
    *
    * Distance correlation measures both linear and non-linear association
    * between two variables. Unlike Pearson (linear only) and Spearman
    * (monotonic only), dCor can detect arbitrary dependence structures.
    * dCor = 0 if and only if the variables are independent (for finite
    * second moments).
    *
    * The algorithm computes pairwise Euclidean distance matrices for x and y,
    * double-centers them (subtracting row, column, and grand means), then
    * computes dCov²(X,Y), dVar²(X), and dVar²(Y) as the mean of the
    * element-wise products. Finally: dCor = √(dCov²/(√(dVar²(X)·dVar²(Y)))).
    *
    * @param pairs a sequence of (x, y) value pairs, with at least 2 pairs
    * @return the distance correlation in [0, 1] (0 = independent, 1 = dependent)
    */
  def distanceCorrelation(pairs: Seq[(Double, Double)]): Double =
    val n = pairs.length
    val xs = pairs.map(_._1)
    val ys = pairs.map(_._2)

    def distMatrix(vs: Seq[Double]): Array[Array[Double]] =
      Array.tabulate(n, n)((i, j) => math.abs(vs(i) - vs(j)))

    def doubleCenter(d: Array[Array[Double]]): Array[Array[Double]] =
      val rowMeans = d.map(row => row.sum / n)
      val colMeans = Array.tabulate(n)(j => (0 until n).map(i => d(i)(j)).sum / n)
      val grandMean = rowMeans.sum / n
      Array.tabulate(n, n)((i, j) => d(i)(j) - rowMeans(i) - colMeans(j) + grandMean)

    def meanProduct(a: Array[Array[Double]], b: Array[Array[Double]]): Double =
      var s = 0.0
      for i <- 0 until n; j <- 0 until n do s += a(i)(j) * b(i)(j)
      s / (n * n)

    val aCenter = doubleCenter(distMatrix(xs))
    val bCenter = doubleCenter(distMatrix(ys))
    val dCovSq = meanProduct(aCenter, bCenter)
    val dVarXSq = meanProduct(aCenter, aCenter)
    val dVarYSq = meanProduct(bCenter, bCenter)
    if dVarXSq <= 0 || dVarYSq <= 0 then 0.0
    else math.sqrt(dCovSq / math.sqrt(dVarXSq * dVarYSq))

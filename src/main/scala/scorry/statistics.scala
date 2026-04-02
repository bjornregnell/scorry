package scorry

object SummaryStatistics:

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

  def parseDoubles(input: String): Seq[Double] =
    input.trim.split("\\s+").filter(_.nonEmpty).map(_.toDouble)

  def parsePairs(input: String): Seq[(Double, Double)] =
    input.split(";").map(_.trim).filter(_.nonEmpty).map: pair =>
      val nums = pair.split("\\s+").map(_.toDouble)
      (nums(0), nums(1))

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

  def spearmanCorrelation(pairs: Seq[(Double, Double)]): Double =
    val rankedPairs = ranks(pairs.map(_._1)).zip(ranks(pairs.map(_._2)))
    pearsonCorrelation(rankedPairs)

  // Regularized incomplete beta function via continued fraction (Lentz's method)
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

  private def lnGamma(x: Double): Double =
    val cof = Seq(76.18009172947146, -86.50532032941677, 24.01409824083091,
      -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5)
    val y = x; val tmp = x + 5.5 - (x + 0.5) * math.log(x + 5.5)
    val ser = 1.000000000190015 + cof.zipWithIndex.map((c, i) => c / (y + 1 + i)).sum
    -tmp + math.log(2.5066282746310005 * ser / x)

  private def betai(a: Double, b: Double, x: Double): Double =
    if x <= 0 then 0.0
    else if x >= 1 then 1.0
    else
      val bt = math.exp(lnGamma(a + b) - lnGamma(a) - lnGamma(b) + a * math.log(x) + b * math.log(1 - x))
      if x < (a + 1) / (a + b + 2) then bt * betacf(a, b, x) / a
      else 1.0 - bt * betacf(b, a, 1 - x) / b

  def tTestPValue(t: Double, df: Int): Double =
    val x = df.toDouble / (df + t * t)
    betai(df / 2.0, 0.5, x)

  def pairedTTest(pairs: Seq[(Double, Double)]): (Double, Int, Double) =
    val diffs = pairs.map((x, y) => x - y)
    val n = diffs.length
    val meanD = mean(diffs)
    val sdD = math.sqrt(diffs.map(d => (d - meanD) * (d - meanD)).sum / (n - 1))
    val t = meanD / (sdD / math.sqrt(n))
    val df = n - 1
    (t, df, tTestPValue(t, df))

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

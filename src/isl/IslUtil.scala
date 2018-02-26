package isl

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.IOException
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.util.logging.Logger

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

import isl.Conversions.convertLambdaToVoidCallback1
import isl.Conversions.convertLambdaToVoidCallback2
import isl.Isl.TypeAliases.T_IN
import isl.Isl.TypeAliases.T_OUT
import isl.Isl.TypeAliases.T_PAR
import isl.Isl.TypeAliases.T_SET

object Isl {

  val myLogger : Logger = Logger.getLogger("")

  def initCtx() : isl.Ctx = {
    return isl.Ctx.alloc()
  }

  final lazy val ctx = initCtx()

  // names are intended to be imported (using "import exastencils.polyhedron.Isl.TypeAliases._")
  object TypeAliases {
    final val T_PAR = isl.DimType.Param
    final val T_SET = isl.DimType.Set
    final val T_IN = isl.DimType.In
    final val T_OUT = isl.DimType.Out
    final val T_CST = isl.DimType.Cst
    final val T_DIV = isl.DimType.Div
  }

  def simplify(uset : isl.UnionSet) : isl.UnionSet = {
    if (uset == null)
      return null
    return uset.coalesce()
  }

  def simplify(umap : isl.UnionMap) : isl.UnionMap = {
    if (umap == null)
      return null
    return umap.coalesce()
  }

  def simplify(set : isl.Set) : isl.Set = {
    if (set == null)
      return null
    return set.coalesce()
  }

  def simplify(map : isl.Map) : isl.Map = {
    if (map == null)
      return null
    return map.coalesce()
  }

  def simplify(set : isl.BasicSet) : isl.BasicSet = {
    if (set == null)
      return null
    return set.removeRedundancies()
  }

  def simplify(map : isl.BasicMap) : isl.BasicMap = {
    if (map == null)
      return null
    return map.removeRedundancies()
  }

  implicit def islVal2int(v : isl.Val) : Int = {
    return v.getNumSi().toInt
  }

  /**
    * Bounds all dimensions of {@code s} to the interval <i>[min, max]</i>.
    * @return The transformed set.
    */
  def boundDims(s : isl.Set, min : Int, max : Int) : isl.Set = {
    var result : isl.Set = s

    for (i <- 0 until s.dim(TypeAliases.T_SET)) {
      var minConstr : isl.Constraint = isl.Constraint.allocInequality(isl.LocalSpace.fromSpace(s.getSpace))
      minConstr = minConstr.setCoefficientSi(TypeAliases.T_SET, i, 1)
      minConstr = minConstr.setConstantSi(-min)
      result = result.addConstraint(minConstr)

      var maxConstr = isl.Constraint.allocInequality(isl.LocalSpace.fromSpace(s.getSpace))
      maxConstr = maxConstr.setCoefficientSi(TypeAliases.T_SET, i, -1)
      maxConstr = maxConstr.setConstantSi(max)
      result = result.addConstraint(maxConstr)
    }
    return result
  }

  /**
    * Calculates the minimal bounding box for {@code s}.
    */
  def getBoundingBox(s : isl.Set) : isl.BasicSet = {
    var bBox : isl.BasicSet = isl.BasicSet.universe(s.getSpace)
    val lSpace : isl.LocalSpace = isl.LocalSpace.fromSpace(s.getSpace)

    for (dim : Int <- 0 until s.dim(TypeAliases.T_SET)) {
      var max : isl.Val = null
      s.dimMax(dim).foreachPiece((_ : isl.Set, piece : isl.Aff) =>
        max = piece.getConstantVal)
      var min : isl.Val = null
      s.dimMin(dim).foreachPiece((_ : isl.Set, piece : isl.Aff) =>
        min = piece.getConstantVal)
      var minConstr : isl.Constraint = isl.Constraint.allocInequality(lSpace)
      minConstr = minConstr.setCoefficientSi(TypeAliases.T_SET, dim, 1)
      minConstr = minConstr.setConstantSi((-min.getNumSi).toInt)
      var maxConstr : isl.Constraint = isl.Constraint.allocInequality(lSpace)
      maxConstr = maxConstr.setCoefficientSi(TypeAliases.T_SET, dim, -1)
      maxConstr = maxConstr.setConstantSi(max.getNumSi.toInt)
      bBox = bBox.addConstraint(minConstr)
      bBox = bBox.addConstraint(maxConstr)
    }
    return bBox
  }

  /**
    * Calculates the number of points in {@code s} as an {@code isl.PwQpolynomial}.
    */
  def islSetCountNPoints(barvinokBinary : File, barvinokLibraryPath : File,
    s : isl.Set) : isl.PwQpolynomial = {
    val sStr : String = s.toString()
    val barvinokPb : ProcessBuilder = new ProcessBuilder(barvinokBinary
      .getAbsolutePath, (sStr.length() + 1).toString)
    barvinokPb.environment().put("LD_LIBRARY_PATH", barvinokLibraryPath
      .getAbsolutePath)
    val barvinok : Process = try {
      barvinokPb.start()
    } catch {
      case e : IOException => {
        val msg = "Failed to start " + barvinokBinary
        myLogger.warning(msg)
        val ex : RuntimeException = new RuntimeException(msg)
        ex.initCause(e)
        throw ex
      }
    }
    val stdin : BufferedWriter = new BufferedWriter(new OutputStreamWriter(
      barvinok.getOutputStream))
    val stderr : BufferedReader = new BufferedReader(new InputStreamReader(
      barvinok.getErrorStream))
    val stdout : BufferedReader = new BufferedReader(new InputStreamReader(
      barvinok.getInputStream))
    try {
      stdin.write(sStr)
      stdin.newLine()
      stdin.flush()
      stdin.close()
    } catch {
      case e : IOException => {
        val msg = "Failed to communicate with the barvinok process. input: " + sStr
        myLogger.warning(msg)
        barvinok.destroyForcibly()
        val ex : RuntimeException = new RuntimeException(msg)
        ex.initCause(e)
        throw ex
      }
    }
    val nElemsPolyStr : String = try {
      stdout.readLine()
    } catch {
      case e : IOException => {
        val msg = "Failed to read the response from the barvinok process. input: " + sStr
        myLogger.warning(msg + ": " + s)
        barvinok.destroyForcibly()
        throw new RuntimeException(msg)
      }
    }
    if (nElemsPolyStr == null) {
      val errLog : String = readAll(stderr)
      barvinok.destroyForcibly()
      val msg = "The barvinok binary died unexpectedly: " + errLog + " input: " + sStr
      myLogger.warning(msg + ": " + s)
      throw new IllegalStateException(msg)
    }
    barvinok.waitFor()
    if (barvinok.exitValue() != 0) {
      val msg = "The barvinok binary didn't return normally. input: " + sStr
      myLogger.warning(msg + ": " + s)
      throw new IllegalStateException(msg)
    }
    val nElemsPoly : isl.PwQpolynomial = try {
      isl.PwQpolynomial.readFromStr(s.getCtx, nElemsPolyStr)
    } catch {
      case e : IslException => {
        val msg = "Couldn't interpret the output of " + barvinokBinary +
          " as an Isl piecewise quasi polynomial: " + nElemsPolyStr
        myLogger.warning(msg + ": " + s + " - " + nElemsPolyStr)
        throw new RuntimeException(msg)
      }
    }
    return nElemsPoly
  }

  /**
    * Reads everything from a given buffered reader line by line. Concatenates
    * the lines to a {@code String} separated by line breaks.
    */
  private def readAll(r : BufferedReader) : String = {
    val sb : StringBuilder = new StringBuilder()
    var line = r.readLine()
    while (line != null) {
      sb.append(line)
      sb.append('\n')
      line = r.readLine()
    }
    return sb.toString()
  }

  /**
    * Check whether the given {@code isl.UnionMap} wraps a nested map.
    */
  def islUnionMapRangeIsWrapping(m : isl.UnionMap) : Boolean = {
    return m.sample().rangeIsWrapping()
  }

  /**
    * Check whether the given {@code isl.Map} wraps a nested map.
    */
  def islMapRangeIsWrapping(m : isl.Map) : Boolean = {
    return m.rangeIsWrapping()
  }

  def islSetContains(set : isl.Set, pt : Array[Int]) : Boolean = {
    var islPt : isl.Point = isl.Point.zero(set.getSpace)
    val ctx = set.getCtx()
    for ((p, i) <- pt.view.zipWithIndex)
      islPt = islPt.setCoordinateVal(TypeAliases.T_SET, i, isl.Val.intFromSi(ctx, p))
    return !isl.Set.fromPoint(islPt).intersect(set).isEmpty()
  }

  def islSetContains(set : isl.Set, pt : Array[BigInt]) : Boolean = {
    var islPt : isl.Point = isl.Point.zero(set.getSpace)
    val ctx = set.getCtx()
    for ((p, i) <- pt.view.zipWithIndex)
      islPt = islPt.setCoordinateVal(TypeAliases.T_SET, i, isl.Val.fromBigInteger(ctx, p.bigInteger))
    return !isl.Set.fromPoint(islPt).intersect(set).isEmpty()
  }

  /**
    * Takes a multi-dimensional Isl union map and splits it into a list of
    * one-dimensional union maps.
    */
  def splitMultiDimUnionMap(m : isl.UnionMap) : List[isl.UnionMap] = {
    val dim2Maps : ArrayBuffer[HashSet[isl.Map]] = ArrayBuffer.empty

    m.foreachMap((m : isl.Map) => {
      val nDims : Int = m.getSpace.dim(TypeAliases.T_OUT)
      for (i <- 0 until nDims) {
        var dimMap : isl.Map = m.projectOut(TypeAliases.T_OUT, 0, i)
        dimMap = dimMap.projectOut(TypeAliases.T_OUT, 1, nDims - i - 1)
        if (dim2Maps.size <= i)
          dim2Maps.append(HashSet.empty)
        dim2Maps(i).add(dimMap)
      }
    })
    return dim2Maps.map { maps =>
      {
        maps.foldLeft(isl.UnionMap.empty(m.getSpace))((uMap : isl.UnionMap,
          m : isl.Map) => {
          uMap.addMap(m)
        })
      }
    }.toList
  }

  /**
    * Takes a list of single-dimensional Isl union maps and combines them into
    * one multi-dimensional union map. The single union maps must have similar
    * structure. Each tuple name must occur at most once in each union map.
    * Otherwise the result produced by this method is incorrect. The
    * given list must not be empty.
    *
    */
  def buildMultiDimUnionMap(maps : List[isl.UnionMap]) : isl.UnionMap = {
    if (maps.isEmpty)
      throw new IllegalArgumentException("maps must not be empty.")

    // separate the maps in each dimension
    val mapLists : List[List[isl.Map]] = maps.map(splitUnionMap)

    // group maps with equal tuple names. Retain their order.
    val tupleNames2MapLists : HashMap[String, ListBuffer[isl.Map]] = HashMap.empty

    for (m <- mapLists.head)
      tupleNames2MapLists.put(m.getTupleName(TypeAliases.T_IN), ListBuffer(m))

    for (l <- mapLists.tail)
      for (m <- l) {
        val tName : String = m.getTupleName(TypeAliases.T_IN)
        if (!tupleNames2MapLists.contains(tName))
          throw new IllegalArgumentException(
            "The given union maps are not of uniform structure.")
        tupleNames2MapLists(tName).append(m)
      }

    // build a multi-dimensional map for each tuple name
    val multiDimMaps : List[isl.Map] = (for ((tName, maps) <- tupleNames2MapLists) yield {
      var sp = maps.head.getSpace
      sp = sp.addDims(TypeAliases.T_OUT, maps.length - 1)
      var mAff : isl.MultiAff = isl.MultiAff.zero(sp)
      for ((m, i) <- maps.zipWithIndex) {
        mAff = mAff.setAff(i, islMap2Aff(m))
      }
      isl.Map.fromMultiAff(mAff)
    }).toList

    // construct the resulting union map by unifying the elements of multiDimMaps
    return multiDimMaps
      .foldLeft(isl.UnionMap.empty(maps.head.getSpace))((u : isl.UnionMap, m : isl.Map) => u.addMap(m))
  }

  /**
    *  Transforms the given {@code isl.Map} into an {@code isl.Aff}.
    *  @throws IllegalArgumentException thrown if the requested transformation is
    *  impossible.
    */
  def islMap2Aff(m : isl.Map) : isl.Aff = {
    if (m.dim(TypeAliases.T_OUT) != 1)
      throw new IllegalArgumentException("m has more than one output dimension: " + m)
    val pwMAff : isl.PwMultiAff = isl.PwMultiAff.fromMap(m)
    var nPieces : Int = 0
    var firstPiece : isl.MultiAff = null
    pwMAff.foreachPiece((s : isl.Set, p : isl.MultiAff) => {
      if (nPieces > 0)
        throw new IllegalArgumentException("m cannot be represented as a multi aff.")
      nPieces += 1
      firstPiece = p
    })
    if (firstPiece == null)
      throw new IllegalArgumentException("m cannot be represented as a multi aff.")
    return firstPiece.getAff(0)
  }

  /**
    * Splits {@code m} into a list of maps. Coalesces the maps.
    */
  def splitUnionMap(m : isl.UnionMap) : List[isl.Map] = {
    var result : List[isl.Map] = List.empty
    m.coalesce.foreachMap((m1 : isl.Map) => {
      result ::= m1
    })
    return result
  }

  /**
    * Splits {@code m} into a list of basic maps.  Coalesces the basic maps.
    */
  
  def splitMap(m : isl.Map) : List[isl.BasicMap] = {
    var result : List[isl.BasicMap] = List.empty
    m.coalesce.foreachBasicMap((m1 : isl.BasicMap) => {
      result ::= m1
    })
    return result
  }
  
  /**
    * Splits {@code m} into a list of maps.
    */
  def splitUnionMapNoCoalesce(m : isl.UnionMap) : List[isl.Map] = {
    var result : List[isl.Map] = List.empty
    m.foreachMap((m1 : isl.Map) => {
      result ::= m1
    })
    return result
  }
  
  /**
    * Splits {@code m} into a list of basic maps.
    */
  def splitMapNoCoalesce(m : isl.Map) : List[isl.BasicMap] = {
    var result : List[isl.BasicMap] = List.empty
    m.foreachBasicMap((m1 : isl.BasicMap) => {
      result ::= m1
    })
    return result
  }

  /**
    * Splits {@code s} into a list of sets. Coalesces the sets.
    */
  def splitUnionSet(s : isl.UnionSet) : List[isl.Set] = {
    var result : List[isl.Set] = List.empty
    s.coalesce.foreachSet((s1 : isl.Set) => {
      result ::= s1
    })
    return result
  }

  /**
    * Splits {@code s} into a list of basic sets. Coalesces the basic sets.
    */
  def splitSet(s : isl.Set) : List[isl.BasicSet] = {
    var result : List[isl.BasicSet] = List.empty
    s.coalesce.foreachBasicSet((s1 : isl.BasicSet) => {
      result ::= s1
    })
    return result
  }
  
  /**
    * Splits {@code s} into a list of sets.
    */
  def splitUnionSetNoCoalesce(s : isl.UnionSet) : List[isl.Set] = {
    var result : List[isl.Set] = List.empty
    s.foreachSet((s1 : isl.Set) => {
      result ::= s1
    })
    return result
  }

  /**
    * Splits {@code s} into a list of basic sets.
    */
  def splitSetNoCoalesce(s : isl.Set) : List[isl.BasicSet] = {
    var result : List[isl.BasicSet] = List.empty
    s.foreachBasicSet((s1 : isl.BasicSet) => {
      result ::= s1
    })
    return result
  }

  /**
    * Checks whether {@code m} is one-dimensional. Throws an
    * {@code IllegalArgumentException} if this is not the case.
    */
  def check1D(m : isl.UnionMap) {
    m.foreachMap((m1 : isl.Map) => {
      if (m1.nOut() != 1)
        throw new IllegalArgumentException("m must have exactly one output dimension: " + m1.nOut())
    })
  }

  /**
    * Collect the tuple names of the sets contained by a union set.
    */
  def islUnionSetGetTupleNames(s : isl.UnionSet) : scala.Predef.Set[String] = {
    val result : HashSet[String] = HashSet.empty
    s.foreachSet((s : isl.Set) => {
      result.add(s.getTupleName)
    })
    return result.toSet
  }

  /**
    * Collect the tuple names of the domains of the maps contained by a union map.
    */
  def islUnionMapGetTupleNames(m : isl.UnionMap) : scala.Predef.Set[String] = {
    val result : HashSet[String] = HashSet.empty
    m.foreachMap((mm : isl.Map) => {
      result.add(mm.getTupleName(T_IN))
    })
    return result.toSet
  }

  /**
    * Compute the set of vectors that are linearly dependent on the given vectors.
    */
  def computeLinDepSet(vectors : Iterable[Array[BigInt]], ctx : isl.Ctx) : Option[isl.BasicSet] = {
    if (vectors.isEmpty)
      return None

    val nrPoints : Int = vectors.size
    val coeffsWithIndex : Iterable[(Array[BigInt], Int)] = vectors.view.zipWithIndex
    val nDims : Int = vectors.head.length
    //implicit val int2Val = (i : Int) => isl.Val.intFromSi(ctx, i)

    var mAff = isl.MultiAff.zero(isl.Space.alloc(ctx, 0, nrPoints, nDims))
    for (pos <- 0 until nDims) {
      var aff = isl.Aff.zeroOnDomain(isl.LocalSpace.fromSpace(isl.Space.setAlloc(ctx, 0, nrPoints)))
      for ((cs, i) <- coeffsWithIndex)
        aff = aff.setCoefficientVal(T_IN, i, isl.Val.fromBigInteger(ctx, cs(pos).bigInteger))
      mAff = mAff.setAff(pos, aff)
    }
    var linDepSpace : isl.BasicSet = isl.BasicMap.fromMultiAff(mAff).range()
    // remove existentially qualified variables in linDepSace (with removeDivs)
    //   because there could be holes in the space... [1 0 | 0 2] does not span the full 2D space
    linDepSpace = linDepSpace.removeDivs()

    return Some(linDepSpace)
  }

  /**
    * Returns {@code true} iff {@code m} always returns 0. Returns false if {@code m} is empty.
    */
  def islMapRangeIsZero(m : isl.Map) : Boolean = {

    if (!m.isSingleValued())
      return false

    if (m.isEmpty())
      return false
    isl.PwMultiAff.fromMap(m).foreachPiece((_ : isl.Set, mAff : isl.MultiAff) => {
      val nAff : Int = mAff.dim(T_OUT)
      for (i <- 0 until nAff) {
        val aff : isl.Aff = mAff.getAff(i)
        for (j <- 0 until aff.dim(T_PAR))
          if (aff.getCoefficientVal(T_PAR, j).getNumSi != 0)
            return false
        for (j <- 0 until aff.dim(T_IN))
          if (aff.getCoefficientVal(T_IN, j).getNumSi != 0)
            return false

        if (aff.getConstantVal.getNumSi != 0)
          return false
      }
    })
    return true
  }

  /**
    * Returns {@code true} iff each map in {@code m} always returns 0. Returns false if {@code m} is empty.
    */
  def islUnionMapRangeIsZero(m : isl.UnionMap) : Boolean = {
    if (m.isEmpty())
      return false
    m.foreachMap((m : isl.Map) => {
      if (!islMapRangeIsZero(m))
        return false
    })
    return true
  }

  /**
    * Adds {@code m} to {@code uMap}. If {@code uMap == null} a new union map is created from {@code m}.
    */
  def addMapToUnionMap(uMap : isl.UnionMap, m : isl.Map) : isl.UnionMap = {

    if (uMap == null) {
      isl.UnionMap.fromMap(m)
    } else {
      uMap.addMap(m)
    }
  }

  /**
    * Filter the given union set for sets with tuple names from {@code names}.
    */
  def islUnionSetFilter(s : isl.UnionSet, names : scala.Predef.Set[String]) : isl.UnionSet = {
    var result : isl.UnionSet = isl.UnionSet.empty(s.getSpace)
    s.foreachSet((s : isl.Set) => {
      if (names.contains(s.getTupleName))
        result = result.addSet(s)
    })
    return result
  }

  /**
    * Filter the given union map for maps with input tuple names from {@code names}.
    */
  def islUnionMapFilter(m : isl.UnionMap, names : scala.Predef.Set[String]) : isl.UnionMap = {
    var result : isl.UnionMap = isl.UnionMap.empty(m.getSpace)
    m.foreachMap((mm : isl.Map) => {
      if (names.contains(mm.getTupleName(T_IN)))
        result = result.addMap(mm)
    })
    return result
  }

  /**
    * Two elements x from {@code dom1} and y from {@code dom2} are in relation iff {@code m1(x) == m2(y)}. The resulting
    * constraint reduces the cross-product of {@code dom1} and {@code dom2} to this relation. {@code m1} and {@code m2}
    * must not contain existencially quantified variables.
    *
    * @param dom1 domain of {@code m1}.
    * @param dom2 domain of {@code m2}.
    * @param m1 first 1-d map
    * @param m2 second 1-d map
    */
  def buildEqConstrFromMaps(dom1 : isl.Set, dom2 : isl.Set, m1 : isl.Map, m2 : isl.Map) : isl.Constraint = {
    val crossProd : isl.Map = isl.Map.fromDomainAndRange(dom1, dom2)
    val lSpace : isl.LocalSpace = isl.LocalSpace.fromSpace(crossProd.getSpace)

    val aff1 : isl.Aff = Isl.islMap2Aff(m1)
    val aff2 : isl.Aff = Isl.islMap2Aff(m2)

    /*
     * construct aff1 - aff2 == 0
     */
    var constr : isl.Constraint = isl.Constraint.allocEquality(lSpace)

    def setCoeffs(aff : isl.Aff, dimType : isl.DimType, negate : Boolean, destDimType : isl.DimType) {
      for (i <- 0 until aff.dim(dimType)) {
        var coeff = aff.getCoefficientVal(dimType, i)
        if (negate)
          coeff = coeff.neg
        constr = constr.setCoefficientVal(destDimType, i, coeff)
      }
    }

    // coeffs of input variables from aff1 are input variable coeffs
    setCoeffs(aff1, T_IN, false, T_IN)

    // coeffs of input variables from aff2 are output variable coeffs
    setCoeffs(aff2, T_IN, true, T_OUT)

    // Subtract coefficients of structure parameters and constants
    for (i <- 0 until aff1.dim(T_PAR)) {
      val c1 : isl.Val = aff1.getCoefficientVal(T_PAR, i)
      val c2 : isl.Val = aff2.getCoefficientVal(T_PAR, i)
      constr = constr.setCoefficientVal(T_PAR, i, c1.sub(c2))
    }

    constr = constr.setConstantVal(aff1.getConstantVal.sub(aff2.getConstantVal))
    return constr
  }

  def constructHappensBeforeMap(domain : isl.UnionSet, sched : isl.UnionMap) : isl.UnionMap = {
    val stmts : Predef.Set[String] = Isl.islUnionSetGetTupleNames(domain)
    var result : isl.UnionMap = isl.UnionMap.empty(sched.getSpace)
    for (s1 : String <- stmts) {
      for (s2 : String <- stmts) {
        val sched1 : isl.Map = isl.Map.fromUnionMap(Isl.islUnionMapFilter(sched, Predef.Set(s1)))
        val sched2 : isl.Map = isl.Map.fromUnionMap(Isl.islUnionMapFilter(sched, Predef.Set(s2)))
        val dom1 : isl.Set = isl.Set.fromUnionSet(Isl.islUnionSetFilter(domain, Predef.Set(s1)))
        val dom2 : isl.Set = isl.Set.fromUnionSet(Isl.islUnionSetFilter(domain, Predef.Set(s2)))
        val m : isl.Map = constructHappensBeforeMap(dom1, dom2, sched1, sched2)
        result = result.addMap(m)
      }
    }
    return result
  }

  private def constructHappensBeforeMap(dom1 : isl.Set, dom2 : isl.Set, sched1 : isl.Map, sched2 : isl.Map) : isl.Map = {
    val result : isl.Map = isl.Map.fromDomainAndRange(dom1, dom2)
    return result.addConstraint(Isl.buildHappensBeforeConstrFromMaps(dom1, dom2, sched1, sched2))
  }

  /**
    * Two elements x from {@code dom1} and y from {@code dom2} are in relation iff {@code m1(x) < m2(y)}. The resulting
    * constraint reduces the cross-product of {@code dom1} and {@code dom2} to this relation. {@code m1} and {@code m2}
    * must not contain existencially quantified variables.
    *
    * @param dom1 domain of {@code m1}.
    * @param dom2 domain of {@code m2}.
    * @param m1 first 1-d map
    * @param m2 second 1-d map
    */
  def buildHappensBeforeConstrFromMaps(dom1 : isl.Set, dom2 : isl.Set, m1 : isl.Map, m2 : isl.Map) : isl.Constraint = {
    val crossProd : isl.Map = isl.Map.fromDomainAndRange(dom1, dom2)
    val lSpace : isl.LocalSpace = isl.LocalSpace.fromSpace(crossProd.getSpace)

    val aff1 : isl.Aff = Isl.islMap2Aff(m1)
    val aff2 : isl.Aff = Isl.islMap2Aff(m2)

    /*
     * construct aff2 - aff1 >= 1
     */
    var constr : isl.Constraint = isl.Constraint.allocInequality(lSpace)

    def setCoeffs(aff : isl.Aff, dimType : isl.DimType, negate : Boolean, destDimType : isl.DimType) {
      for (i <- 0 until aff.dim(dimType)) {
        var coeff = aff.getCoefficientVal(dimType, i)
        if (negate)
          coeff = coeff.neg
        constr = constr.setCoefficientVal(destDimType, i, coeff)
      }
    }

    // coeffs of input variables from aff1 are input variable coeffs
    setCoeffs(aff1, T_IN, true, T_IN)

    // coeffs of input variables from aff2 are output variable coeffs
    setCoeffs(aff2, T_IN, false, T_OUT)

    // Subtract coefficients of structure parameters and constants
    for (i <- 0 until aff1.dim(T_PAR)) {
      val c1 : isl.Val = aff1.getCoefficientVal(T_PAR, i)
      val c2 : isl.Val = aff2.getCoefficientVal(T_PAR, i)
      constr = constr.setCoefficientVal(T_PAR, i, c2.sub(c1))
    }

    constr = constr.setConstantVal(aff2.getConstantVal.sub(aff1.getConstantVal).sub(isl.Val.one(ctx)))
    return constr
  }

  /**
    * If a map contains only one basic map it can be transformed into a basic map.
    */
  def islBasicMapFromMap(m : Map) : isl.BasicMap = {
    var bMap : isl.BasicMap = null
    var nBMaps : Int = 0

    m.foreachBasicMap((bm : isl.BasicMap) => {
      if (nBMaps > 0)
        throw new IllegalArgumentException("m contains more than one basic map: " + m)
      nBMaps += 1
      bMap = bm
    })
    return bMap
  }

  def islUnionMapRemoveDivs(m : isl.UnionMap) : isl.UnionMap = {
    var result : isl.UnionMap = isl.UnionMap.empty(m.getSpace)
    m.foreachMap((mm : isl.Map) => {
      result = result.addMap(mm.removeDivs())
    })
    return result
  }
  
  /**
   * Check whether input dimension {@code dim} is constant.
   */
  def isConstInputDim(m : isl.Map, dim : Int) : Boolean = {
    var domain : isl.Set = m.domain()
    domain = domain.projectOut(T_SET, 0, dim)
    domain = domain.projectOut(T_SET, 1, domain.dim(T_SET) - 1)
    return domain.isSingleton()
  }
}

package mapfun.osm

import mapfun.geo.*
import munit.FunSuite

class LongMapSuite extends FunSuite:

  test("LongIntMap stores, overwrites and reports absence") {
    val m = LongIntMap(16)
    assertEquals(m.getOrElse(42L, -1), -1)
    m.put(42L, 7)
    assertEquals(m.getOrElse(42L, -1), 7)
    m.put(42L, 9)
    assertEquals(m.getOrElse(42L, -1), 9)
    assertEquals(m.size, 1)
    assert(m.contains(42L))
    assert(!m.contains(43L))
  }

  test("addTo accumulates, treating a missing key as zero") {
    val m = LongIntMap(16)
    assertEquals(m.addTo(100L, 1), 1)
    assertEquals(m.addTo(100L, 1), 2)
    assertEquals(m.addTo(100L, 3), 5)
    assertEquals(m.getOrElse(100L, 0), 5)
  }

  test("LongIntMap survives growth and keeps every entry") {
    val m = LongIntMap(16)
    val n = 50000
    // Ids in the range OSM actually uses, so the hash gets a realistic workout.
    (1 to n).foreach(i => m.put(4200000000L + i * 7, i))
    assertEquals(m.size, n)
    (1 to n).foreach(i => assertEquals(m.getOrElse(4200000000L + i * 7, -1), i, s"entry $i"))
  }

  test("hash slots spread over the whole table at every capacity, not just the first 2^24") {
    // Sequential ids, like the ones OSM hands out within an extract.
    Seq(1 << 4, 1 << 16, 1 << 24, 1 << 26, 1 << 30).foreach { capacity =>
      val shift = LongHash.shiftFor(capacity)
      val slots = (1L to 100_000L).map(id => LongHash.slot(id + 5_000_000_000L, shift))
      assert(slots.forall(s => s >= 0 && s < capacity), s"capacity $capacity: slot out of range")
      // A fixed 24-bit hash never reaches the upper half of a 2^26 table.
      assert(slots.exists(_ >= capacity / 2), s"capacity $capacity: upper half never used")
    }
  }

  test("LongIntMap iterates exactly its entries") {
    val m = LongIntMap(16)
    (1 to 1000).foreach(i => m.put(i.toLong, i * 2))
    var seen = 0
    var sum = 0L
    m.foreach { (k, v) =>
      seen += 1
      sum += v
      assertEquals(v, (k * 2).toInt)
    }
    assertEquals(seen, 1000)
    assertEquals(sum, (1 to 1000).map(_ * 2L).sum)
  }

  test("coordinate packing round-trips at OSM's 1e-7 degree precision") {
    val cases = Seq(
      (41.8240123, -71.4128456),
      (-33.8688, 151.2093),
      (0.0, 0.0),
      (85.0, 179.9999999),
      (-85.0, -180.0)
    )
    cases.foreach { (lat, lon) =>
      val p = LongCoordMap.pack(lat, lon)
      assertEqualsDouble(LongCoordMap.unpackLat(p), lat, 1e-7, s"lat $lat")
      assertEqualsDouble(LongCoordMap.unpackLon(p), lon, 1e-7, s"lon $lon")
    }
  }

  test("LongCoordMap reports missing keys distinguishably") {
    val m = LongCoordMap(16)
    assertEquals(m.getPacked(5L), Long.MinValue)
    m.put(5L, 41.5, -71.5)
    assertNotEquals(m.getPacked(5L), Long.MinValue)
    assertEqualsDouble(LongCoordMap.unpackLat(m.getPacked(5L)), 41.5, 1e-7)
  }

class RoadClassSuite extends FunSuite:

  test("known highway tags resolve, unknown ones do not") {
    assertEquals(RoadClass.fromTag("motorway"), Some(RoadClass.Motorway))
    assertEquals(RoadClass.fromTag("residential"), Some(RoadClass.Residential))
    assertEquals(RoadClass.fromTag("proposed"), None)
    assertEquals(RoadClass.fromTag(""), None)
  }

  test("cars are kept off footways, pedestrians off motorways") {
    assert(TravelProfile.Car.allowed.contains(RoadClass.Residential))
    assert(!TravelProfile.Car.allowed.contains(RoadClass.Footway))
    assert(TravelProfile.Foot.allowed.contains(RoadClass.Footway))
    assert(!TravelProfile.Foot.allowed.contains(RoadClass.Motorway))
  }

  test("max speed bounds every allowed road's speed -- A* admissibility depends on it") {
    TravelProfile.values.foreach { p =>
      p.allowed.foreach { rc =>
        assert(
          p.speedMps(rc) <= p.maxSpeedMps + 1e-9,
          s"$p on $rc travels ${p.speedMps(rc)} but claims max ${p.maxSpeedMps}"
        )
      }
    }
  }

  test("posted maxspeed never pushes a way above the profile's max speed") {
    val kph = (v: Double) => v * 1000.0 / 3600.0
    TravelProfile.values.foreach { p =>
      p.allowed.foreach { rc =>
        Seq(None, Some(kph(5)), Some(kph(50)), Some(kph(130)), Some(kph(200))).foreach { posted =>
          val s = p.speedMps(rc, posted)
          assert(s <= p.maxSpeedMps + 1e-9, s"$p on $rc with $posted travels $s")
        }
      }
    }
  }

  test("posted maxspeed sets car speed, caps cyclists, and is ignored on foot") {
    val kph = (v: Double) => v * 1000.0 / 3600.0
    val rc  = RoadClass.Residential
    assertEqualsDouble(TravelProfile.Car.speedMps(rc, Some(kph(50))), kph(50), 1e-9)
    assertEqualsDouble(TravelProfile.Foot.speedMps(rc, Some(kph(50))), 1.4, 1e-9)
    assertEqualsDouble(TravelProfile.Bike.speedMps(rc, Some(kph(50))), 4.5, 1e-9)
    assertEqualsDouble(TravelProfile.Bike.speedMps(rc, Some(kph(10))), kph(10), 1e-9)
  }

  test("pedestrians ignore one-way restrictions") {
    assert(!TravelProfile.Foot.respectOneway)
    assert(TravelProfile.Car.respectOneway)
  }

/** Exercises the junction contraction directly, with hand-built ways. */
class ContractionSuite extends FunSuite:

  // A line of nodes about 100 m apart, so distances are easy to reason about.
  private def coordsFor(ids: Seq[Long]): LongCoordMap =
    val m = LongCoordMap(64)
    ids.zipWithIndex.foreach { (id, i) =>
      m.put(id, 41.8 + i * 0.0009, -71.4)
    }
    m

  private def way(
      refs: Seq[Long],
      forward: Boolean = true,
      backward: Boolean = true,
      speed: Double = 10.0
  ): RawWay =
    RawWay(refs.toArray, RoadClass.Residential, forward, backward, speed)

  test("intermediate geometry nodes collapse into one edge of the full length") {
    val ids = Seq(1L, 2L, 3L, 4L, 5L)
    val coords = coordsFor(ids)
    // Only the endpoints are junctions; 2, 3 and 4 are just bends in the road.
    val (geo, _) = OsmImporter.assemble(
      ways = Seq(way(ids)),
      coords = coords,
      isJunction = id => id == 1L || id == 5L,
      costModel = CostModel.Distance,
      profile = TravelProfile.Car,
      restrictToLargestScc = false,
      name = "test"
    )

    assertEquals(geo.numNodes, 2)
    assertEquals(geo.numEdges, 2) // one edge each way

    // The single edge must carry the whole polyline's length, not the
    // straight-line distance between the endpoints -- here they coincide
    // because the nodes are collinear, so check against the sum explicitly.
    val expected = (0 until 4)
      .map(i => Haversine.distance(LatLon(41.8 + i * 0.0009, -71.4), LatLon(41.8 + (i + 1) * 0.0009, -71.4)))
      .sum
    assertEqualsDouble(geo.graph.neighbors(0).head._2, expected, 1e-6)
  }

  test("a shared node becomes a junction splitting the way in two") {
    val ids = Seq(1L, 2L, 3L)
    val (geo, _) = OsmImporter.assemble(
      ways = Seq(way(ids)),
      coords = coordsFor(ids),
      isJunction = _ => true, // every node is a junction
      costModel = CostModel.Distance,
      profile = TravelProfile.Car,
      restrictToLargestScc = false,
      name = "test"
    )
    assertEquals(geo.numNodes, 3)
    assertEquals(geo.numEdges, 4) // two segments, both directions
  }

  test("one-way ways produce edges in a single direction") {
    val ids = Seq(1L, 2L)
    val (geo, _) = OsmImporter.assemble(
      ways = Seq(way(ids, forward = true, backward = false)),
      coords = coordsFor(ids),
      isJunction = _ => true,
      costModel = CostModel.Distance,
      profile = TravelProfile.Car,
      restrictToLargestScc = false,
      name = "test"
    )
    assertEquals(geo.numEdges, 1)
    assertEquals(geo.graph.outDegree(0), 1)
    assertEquals(geo.graph.outDegree(1), 0)
  }

  test("oneway=-1 runs against the node order") {
    val ids = Seq(1L, 2L)
    val (geo, _) = OsmImporter.assemble(
      ways = Seq(way(ids, forward = false, backward = true)),
      coords = coordsFor(ids),
      isJunction = _ => true,
      costModel = CostModel.Distance,
      profile = TravelProfile.Car,
      restrictToLargestScc = false,
      name = "test"
    )
    assertEquals(geo.numEdges, 1)
    assertEquals(geo.graph.outDegree(0), 0)
    assertEquals(geo.graph.outDegree(1), 1)
  }

  test("time costing divides length by speed") {
    val ids = Seq(1L, 2L)
    val coords = coordsFor(ids)
    val speed = 25.0
    val (geo, _) = OsmImporter.assemble(
      ways = Seq(way(ids, speed = speed)),
      coords = coords,
      isJunction = _ => true,
      costModel = CostModel.Time,
      profile = TravelProfile.Car,
      restrictToLargestScc = false,
      name = "test"
    )
    val metres = Haversine.distance(LatLon(41.8, -71.4), LatLon(41.8009, -71.4))
    assertEqualsDouble(geo.graph.neighbors(0).head._2, metres / speed, 1e-6)
    assertEquals(geo.metric, WeightMetric.Seconds(TravelProfile.Car.maxSpeedMps))
  }

  test("nodes missing from the extract break the way instead of bridging the gap") {
    // Node 3 has no coordinates -- exactly what happens at a clip boundary.
    val coords = LongCoordMap(64)
    coords.put(1L, 41.8000, -71.4)
    coords.put(2L, 41.8009, -71.4)
    coords.put(4L, 41.8027, -71.4)
    coords.put(5L, 41.8036, -71.4)

    val (geo, _) = OsmImporter.assemble(
      ways = Seq(way(Seq(1L, 2L, 3L, 4L, 5L))),
      coords = coords,
      isJunction = _ => true,
      costModel = CostModel.Distance,
      profile = TravelProfile.Car,
      restrictToLargestScc = false,
      name = "test"
    )
    // 1-2 and 4-5 survive; nothing joins 2 to 4 across the hole.
    assertEquals(geo.numNodes, 4)
    assertEquals(geo.numEdges, 4)
    val ids = geo.positions.map(_.lat).sorted
    assertEquals(ids.length, 4)
    // No edge should span the missing node's gap.
    geo.graph.nodeIds.foreach { u =>
      geo.graph.foreachOutEdge(u) { (v, w) =>
        assert(w < 200.0, s"edge $u->$v of $w m bridges the gap")
      }
    }
  }

  test("a closed loop does not produce a self-loop edge") {
    val coords = LongCoordMap(64)
    coords.put(1L, 41.8000, -71.4000)
    coords.put(2L, 41.8009, -71.4000)
    coords.put(3L, 41.8009, -71.3990)

    // A way that returns to its own start.
    val (geo, _) = OsmImporter.assemble(
      ways = Seq(way(Seq(1L, 2L, 3L, 1L))),
      coords = coords,
      isJunction = id => id == 1L,
      costModel = CostModel.Distance,
      profile = TravelProfile.Car,
      restrictToLargestScc = false,
      name = "test"
    )
    assertEquals(geo.numNodes, 1)
    geo.graph.nodeIds.foreach { u =>
      geo.graph.foreachOutEdge(u)((v, _) => assertNotEquals(u, v, "self-loop emitted"))
    }
  }

  test("SCC restriction drops a one-way stub that cannot be left") {
    val coords = LongCoordMap(64)
    coords.put(1L, 41.8000, -71.4000)
    coords.put(2L, 41.8009, -71.4000)
    coords.put(3L, 41.8018, -71.4000)

    val ways = Seq(
      way(Seq(1L, 2L)),                                     // two-way
      way(Seq(2L, 3L), forward = true, backward = false)     // one-way into a dead end
    )
    val (unrestricted, _) = OsmImporter.assemble(
      ways, coords, _ => true, CostModel.Distance, TravelProfile.Car,
      restrictToLargestScc = false, name = "test"
    )
    assertEquals(unrestricted.numNodes, 3)

    val (restricted, dropped) = OsmImporter.assemble(
      ways, coords, _ => true, CostModel.Distance, TravelProfile.Car,
      restrictToLargestScc = true, name = "test"
    )
    // Node 3 can be entered but never left, so it is its own SCC and goes.
    assertEquals(restricted.numNodes, 2)
    assertEquals(dropped, 1)
  }

  test("after SCC restriction every node reaches every other") {
    val coords = LongCoordMap(64)
    (1 to 6).foreach(i => coords.put(i.toLong, 41.80 + i * 0.001, -71.40))
    val ways = Seq(
      way(Seq(1L, 2L, 3L)),
      way(Seq(3L, 4L)),
      way(Seq(4L, 5L), forward = true, backward = false),
      way(Seq(6L, 1L), forward = true, backward = false)
    )
    val (geo, _) = OsmImporter.assemble(
      ways, coords, _ => true, CostModel.Distance, TravelProfile.Car,
      restrictToLargestScc = true, name = "test"
    )
    val tree = mapfun.algo.Dijkstra.from(geo.graph, 0)
    assertEquals(tree.reachableCount, geo.numNodes)
  }

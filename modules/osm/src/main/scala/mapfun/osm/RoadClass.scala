package mapfun.osm

/** How OpenStreetMap's `highway=*` tag maps onto something routable.
  *
  * OSM has no notion of "speed limit" you can rely on -- `maxspeed` is present
  * on a minority of ways and written in a dozen formats. Real routers fall back
  * to a per-road-class default table like this one. The numbers are ordinary
  * assumptions about US roads, not measurements; changing them changes which
  * route "fastest" picks, which is a good thing to experiment with.
  */
enum RoadClass(val tag: String, val defaultKph: Double, val carAccessible: Boolean):
  case Motorway     extends RoadClass("motorway", 110, true)
  case MotorwayLink extends RoadClass("motorway_link", 70, true)
  case Trunk        extends RoadClass("trunk", 95, true)
  case TrunkLink    extends RoadClass("trunk_link", 60, true)
  case Primary      extends RoadClass("primary", 80, true)
  case PrimaryLink  extends RoadClass("primary_link", 50, true)
  case Secondary    extends RoadClass("secondary", 65, true)
  case SecondaryLink extends RoadClass("secondary_link", 45, true)
  case Tertiary     extends RoadClass("tertiary", 55, true)
  case TertiaryLink extends RoadClass("tertiary_link", 40, true)
  case Unclassified extends RoadClass("unclassified", 45, true)
  case Residential  extends RoadClass("residential", 35, true)
  case LivingStreet extends RoadClass("living_street", 15, true)
  case Service      extends RoadClass("service", 20, true)
  case Road         extends RoadClass("road", 30, true)

  // Not drivable, but routable on foot or by bike -- kept so the same importer
  // can build a pedestrian graph, which is a genuinely different network.
  case Pedestrian extends RoadClass("pedestrian", 5, false)
  case Footway    extends RoadClass("footway", 5, false)
  case Path       extends RoadClass("path", 5, false)
  case Cycleway   extends RoadClass("cycleway", 16, false)
  case Steps      extends RoadClass("steps", 2, false)
  case Track      extends RoadClass("track", 15, false)

  def metresPerSecond: Double = defaultKph * 1000.0 / 3600.0

object RoadClass:
  private val byTag: Map[String, RoadClass] = values.map(rc => rc.tag -> rc).toMap

  def fromTag(tag: String): Option[RoadClass] = byTag.get(tag)

  /** The classes a car may use. */
  val drivable: Set[RoadClass] = values.filter(_.carAccessible).toSet

  /** Everything a person on foot may use -- most drivable roads plus paths. */
  val walkable: Set[RoadClass] =
    values.toSet -- Set(Motorway, MotorwayLink, Trunk, TrunkLink)

  val cyclable: Set[RoadClass] =
    values.toSet -- Set(Motorway, MotorwayLink, Steps, Footway)

/** Which network to extract, and how to cost it. */
enum TravelProfile(val allowed: Set[RoadClass], val respectOneway: Boolean):
  case Car extends TravelProfile(RoadClass.drivable, true)

  /** Pedestrians ignore one-way restrictions, which changes connectivity a lot
    * -- a walking graph out of the same extract is far better connected.
    */
  case Foot extends TravelProfile(RoadClass.walkable, false)
  case Bike extends TravelProfile(RoadClass.cyclable, true)

  /** Speed in metres/second for costing travel time. Pedestrians and cyclists
    * move at their own pace regardless of the road class.
    */
  def speedMps(rc: RoadClass): Double = this match
    case Car  => rc.metresPerSecond
    case Foot => 1.4 // ~5 km/h
    case Bike => math.min(rc.metresPerSecond, 4.5) // ~16 km/h

  /** Speed on one way, given its parsed `maxspeed` tag if it has one.
    *
    * A posted limit is a motor-vehicle limit: it sets a car's speed but only
    * ever slows a cyclist down, and a pedestrian ignores it. The result is
    * clamped to [[maxSpeedMps]] because the A* heuristic divides by that --
    * one 130 km/h motorway tag above the cap would make it overestimate and
    * quietly return non-optimal routes.
    */
  def speedMps(rc: RoadClass, postedMps: Option[Double]): Double =
    val raw = this match
      case Car  => postedMps.getOrElse(speedMps(rc))
      case Foot => speedMps(rc)
      case Bike => postedMps.fold(speedMps(rc))(math.min(_, speedMps(rc)))
    math.min(math.max(0.5, raw), maxSpeedMps)

  /** Fastest possible movement anywhere in this profile's network.
    *
    * A* needs this to convert a straight-line distance into an admissible
    * lower bound on travel time. Overstate it and A* gets slower but stays
    * correct; understate it and A* silently returns non-optimal routes.
    */
  def maxSpeedMps: Double = this match
    case Car  => allowed.map(_.metresPerSecond).maxOption.getOrElse(1.0)
    case Foot => 1.4
    case Bike => 4.5

/** What the resulting edge weights should measure. */
enum CostModel:
  case Distance
  case Time

package com.gilt.handlebars

import org.specs2.mutable._
import com.gilt.handlebars._

class HandlebarsPerfSpec extends Specification {
  "handlebars" should {
    "render a template in less than 5 ms" in {
      val template = PrimaryNavUtils.loadTemplate("./src/test/resource/performance/templates/primarynav.handlebars")
      val context = PrimaryNavUtils.generateContext()
      val handlebars = Handlebars(template)

      val count = 5000
      var totalRuntime = 0l
      var totalSize = 0l

      // val primaryNav = handlebars(context)
      // println(primaryNav)

      println("==== WARMING UP ....")

      for (i <- 0 to count / 10) { // warmup
        val now = System.currentTimeMillis
        val primaryNav = handlebars(context)
        totalSize += primaryNav.size
      }

      println("==== STARTING TEST ....")

      for (i <- 0 to count) {
        val now = System.currentTimeMillis
        val primaryNav = handlebars(context)
        val took = System.currentTimeMillis - now
        totalRuntime += took
        totalSize += primaryNav.size

        //println("Took: [%s]".format(took))
        if (i % 5000 == 0) println("5k!")
      }

      val avg = totalRuntime / count
      println("==== DONE --> AVG TIME: %s ms. Total size: %s".format(avg, totalSize))

      success

    }
  }
}

import scala.collection.LinearSeq
import scala.reflect.BeanProperty
import com.google.common.base.Optional
import java.util.Random

object PrimaryNavUtils {
  import java.io.File
  import scala.io._

  val DefaultTodaySales = TabsViewTab(
    title = "Today's Sales",
    url = Some("http://www.gilt.com/sales/men"),
    footer = Some(WebCta("View All", "/sales/men")),
    columns = List(
      TabsViewItemColumn(List(
        TabsViewItemSection(
          header = Some("Current Sales"),
          elements = List(
            TabsViewItemElement(
              name = "Helmut Lang",
              urlKey = "helmut-lang",
              relativeUrl = "/helmut-lang"
            ),
            TabsViewItemElement(
              name = "sale2",
              urlKey = "sale2",
              relativeUrl = "/sale2"
            ),
            TabsViewItemElement(
              name = "sale3",
              urlKey = "sale3",
              relativeUrl = "/sale3"
            ),
            TabsViewItemElement(
              name = "sale4",
              urlKey = "sale4",
              relativeUrl = "/sale4"
            ),
            TabsViewItemElement(
              name = "sale5",
              urlKey = "sale5",
              relativeUrl = "/sale5"
            ),
            TabsViewItemElement(
              name = "sale6",
              urlKey = "sale6",
              relativeUrl = "/sale6"
            ),
            TabsViewItemElement(
              name = "sale7",
              urlKey = "sale7",
              relativeUrl = "/sale7"
            ),
            TabsViewItemElement(
              name = "sale8",
              urlKey = "sale8",
              relativeUrl = "/sale8"
            ),
            TabsViewItemElement(
              name = "sale9",
              urlKey = "sale9",
              relativeUrl = "/sale9"
            )
          )
        ),

        TabsViewItemSection(
          header = Some("Current Sales"),
          elements = List(
            TabsViewItemElement(
              name = "sale11",
              urlKey = "sale11",
              relativeUrl = "sale11"
            ),
            TabsViewItemElement(
              name = "sale12",
              urlKey = "sale12",
              relativeUrl = "sale12"
            ),
            TabsViewItemElement(
              name = "sale13",
              urlKey = "sale13",
              relativeUrl = "sale13"
            ),
            TabsViewItemElement(
              name = "sale14",
              urlKey = "sale14",
              relativeUrl = "sale14"
            ),
            TabsViewItemElement(
              name = "sale15",
              urlKey = "sale15",
              relativeUrl = "sale15"
            ),
            TabsViewItemElement(
              name = "sale16",
              urlKey = "sale16",
              relativeUrl = "sale16"
            )
          )
        )
      )
    )
  ))

  val DefaultShops = TabsViewTab(
    title = "Shops",
    url = Some("http://www.gilt.com/shops"),
    footer = Some(WebCta("View All", "/shops")),
    columns = List(
      TabsViewItemColumn(List(
        TabsViewItemSection(
          header = Some("Current Sales"),
          elements = List(
            TabsViewItemElement(
              name = "Helmut Lang",
              urlKey = "helmut-lang",
              relativeUrl = "/helmut-lang"
            ),
            TabsViewItemElement(
              name = "sale2",
              urlKey = "sale2",
              relativeUrl = "/sale2"
            ),
            TabsViewItemElement(
              name = "sale3",
              urlKey = "sale3",
              relativeUrl = "/sale3"
            ),
            TabsViewItemElement(
              name = "sale4",
              urlKey = "sale4",
              relativeUrl = "/sale4"
            ),
            TabsViewItemElement(
              name = "sale5",
              urlKey = "sale5",
              relativeUrl = "/sale5"
            )
          )
        )
      ))
    )
  )

  val DefaultWhatsHot = TabsViewTab(
    title = "What's Hot",
    url = Some("http://www.gilt.com/whats-hot"),
    columns = LinearSeq.empty[TabsViewItemColumn]
  )

  val DefaultStylefile = TabsViewTab(
    title = "Stylefile",
    url = Some("http://www.gilt.com/Stylefile"),
    columns = LinearSeq.empty[TabsViewItemColumn]
  )

  val DefaultWebUser = WebUser("Sam")
  val DefaultWebStore = WebStore("women", "Women", "/sale/women")
  val DefaultStore = Store("women", "Women", "/sale/women")
  val DefaultTabs = TabsView(DefaultStore, List(DefaultTodaySales, DefaultShops, DefaultWhatsHot, DefaultStylefile))
  val DefaultIntlPreference = IntlPreference(Country(123, "United States", Iso3166("US")))
  val DefaultLoyaltyUser = LoyaltyUser(5000)

  val rnd = new Random(123)

  val DefaultCountries = {
    val USCA = Region("USCA", "usca")
    val APAC = Region("APAC", "apac")
    val EURO = Region("EURO", "euro")
    val LACA = Region("LACA", "laca")
    val MEAF = Region("MEAF", "meaf")

    CountriesView(
      List(
        CountriesByRegionView(USCA, Country.generateRandomCountries(rnd, 2)),
        CountriesByRegionView(APAC, Country.generateRandomCountries(rnd, 18)),
        CountriesByRegionView(EURO, Country.generateRandomCountries(rnd, 36)),
        CountriesByRegionView(LACA, Country.generateRandomCountries(rnd, 33)),
        CountriesByRegionView(MEAF, Country.generateRandomCountries(rnd, 12))
      ))
  }

  val DefaultUserAccountNavView = UserAccountNavView(
    cartCount = 3,
    displayFirstName = "Sam",
    webUser = DefaultWebUser,
    intlPreferences = Some(DefaultIntlPreference),
    displayCountrySelector = true,
    countries = DefaultCountries,
    loyaltyUser = Optional.of(DefaultLoyaltyUser)
  )

  val DefaultSearchBox = SearchBoxView(DefaultWebStore, enableGlobalSearch = true)

  def loadTemplate(path: String): String = Source.fromFile(new File(path)).getLines.toList.mkString
  def generateContext(): NavigationView = NavigationView(
    webUser = DefaultWebUser,
    store = DefaultWebStore,
    tabs = DefaultTabs,
    userAccountNav = DefaultUserAccountNavView,
    searchBox = DefaultSearchBox
  )
}

// --- model borrowed from commons-view-components ---

case class NavigationView (
  webUser: WebUser,
  store: WebStore,
  tabs: TabsView,
  userAccountNav: UserAccountNavView,
  searchBox: SearchBoxView
)

case class WebUser(name: String) {
  def isNoir: Boolean = true
  def isRegistered: Boolean = true
  def getFirstName = Optional.of(name)
  def isAnonymous: Boolean = false
}

case class WebStore(key: String, displayName: String,  relativeUrl: String, isCurrent: Boolean = false)
case class Store(key: String, displayName: String,  relativeUrl: String)

case class Iso3166(code: String) { def getCode = code }
case class Country(id: Long, name: String, iso: Iso3166) {
  def getIso3166Two = iso
  def getName = name
  def getCountryId = id
}

object Country {
  def generateRandomCountry(rnd: Random): Country = Country(rnd.nextLong(), "country-" + rnd.nextInt(), Iso3166("iso-" + rnd.nextInt))
  def generateRandomCountries(rnd: Random, n: Integer): List[Country] = (for { i <- 0 to n } yield { generateRandomCountry(rnd) }).toList
}

object Region {
  def generateRandomRegion(rnd: Random): Region = Region(displayName = "regions-" + rnd.nextInt(), code = "" + rnd.nextLong)
}

case class IntlPreference(country: Country) {
  def getShipToCountry: Country = country
}

case class LoyaltyUser(points: Long) {
  def spendablePoints = points
}

case class SearchBoxView(store: WebStore, stores: List[WebStore] = List.empty, enableGlobalSearch: Boolean)

case class CountriesView(regions: Seq[CountriesByRegionView])
case class CountriesByRegionView(region: Region, countries: Seq[Country])
case class Region(@BeanProperty displayName: String, @BeanProperty code: String)

case class UserAccountNavView(
  cartCount: Integer,
  displayCart: Boolean = true,
  displayFirstName: String,
  webUser: WebUser,
  countries: CountriesView,
  intlPreferences: Option[IntlPreference],
  displayCountrySelector: Boolean,
  loyaltyUser: Optional[LoyaltyUser]
)

case class WebCta(label: String, url: String)

case class TabsView(navStore: Store, tabs: LinearSeq[TabsViewTab] = LinearSeq.empty[TabsViewTab]) {
  val displayKidsFilter = false
}

case class TabsViewTab(
  title: String, // the main title appearing as the tab's header (e.g. `Today's Sales`, `Gifts`)
  url: Option[String] = None,  // the url to land on
  footer: Option[WebCta] = None, // Cta footer of the tab (e.g. `View All...`)
  columns: LinearSeq[TabsViewItemColumn] = LinearSeq.empty[TabsViewItemColumn] // the columns of this tab
)

case class TabsViewItemColumn(
  sections: LinearSeq[TabsViewItemSection] = LinearSeq.empty[TabsViewItemSection] // single section (e.g. `Ending Soon Sales`)
)

case class TabsViewItemSection(
  elements: LinearSeq[TabsViewItemElement] = LinearSeq.empty[TabsViewItemElement], // single elements for this section
  header: Option[String] = None // header of the section (e.g. `Current Sales`, `Ending Soon Sales`)
)

case class TabsViewItemElement(
  name: String,
  urlKey: String,
  relativeUrl: String
)

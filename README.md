# Flights

Emissions from air travel by UK residents, built from Civil Aviation Authority
statistics, as an origin–destination network of airport pairs with annual
passenger numbers, flight counts, distances, and emissions from 1990 to 2024.

This is the national and route-level half of the flight estimates that appear in
the [Place-Based Carbon Calculator](https://www.carbon.place/pbcc/) on
[Carbon & Place](https://www.carbon.place). It produces the totals and their
breakdown by distance and destination. The separate step that distributes those
totals across neighbourhoods, using the synthetic population and the
relationship between income and flying, lives in
[build](https://github.com/PlaceBasedCarbonCalculator/build).

Flights matter out of proportion to their share of journeys: they are among the
most carbon-intensive things a person can do, and they are highly unequally
distributed, so a neighbourhood average conceals a great deal.

---

## Method

### 1. Flights, from CAA punctuality statistics

`RScripts/flights_import.R` reads the CAA punctuality statistics, which record
flights between each reporting UK airport and each origin or destination airport,
by airline and by scheduled or charter status.

Two details matter:

- **Total flights** are `number_flights_matched + actual_flights_unmatched`, so
  that flights the CAA could not match to a schedule are still counted.
- **Domestic departures are dropped.** A UK-to-UK flight appears twice in the
  data, once as a departure and once as an arrival, so departures where the
  other end is in the United Kingdom are excluded to avoid double counting.

The earliest two files also carry a previous-year comparison column, which is
used to extend the series back a year.

`RScripts/flights_prepare.R` then resolves the remaining direction problem in
domestic data. An A→B and a B→A record describe the same route, so pairs are
collapsed using an order-independent key
(`stplanr::od_id_szudzik`). Airport names are normalised, stripping the many
variants of "International", "Intl", "Airport", and "Apt", and the Crown
Dependencies are separated from the United Kingdom.

### 2. Passengers, from CAA airport statistics

`RScripts/passenger_import.R` and `passenger_prepare.R` read the CAA airport
statistics, which give passenger numbers on each international and domestic
route. These go through the same name normalisation so that the two sources
join.

Passengers rather than flights are what the emissions are calculated from: a
flight's emissions are shared among the people on board, and load factors vary.

### 3. Airports, geocoding, and distances

`RScripts/combine_flights_passengers.R` joins the passenger and flight tables to
a set of geocoded airports and builds the network. Airports the CAA names but
the reference set does not contain are added by hand or geocoded
(`geocode_missing_airports_2024.R`), and `OD2linestring.R` turns each airport
pair into a great-circle line so the result can be mapped.

A few known messes are handled explicitly: Istanbul's traffic moved from Atatürk
to the new airport in 2019 and the two are kept separate before that date;
offshore helicopter traffic appears as a destination called "Oil Rigs"; and
routes the CAA aggregates as "other" are grouped rather than dropped.

### 4. Distance, with a detour factor

Aircraft do not fly great circles. Actual distance flown exceeds the direct
distance because of departure and arrival procedures, airway routing, and
airspace restrictions, and the gap is proportionally largest on short routes.

`RScripts/add_emissions.R` applies detour factors derived from Dobruszkes and
Peeters (2019), [*The magnitude of detours faced by commercial flights: a global
assessment*](https://www.sciencedirect.com/science/article/pii/S0966692318305544),
*Journal of Transport Geography* 79:

| Route length | Detour factor |
| --- | --- |
| Under 1,000 km | 1.143 |
| 1,000 to 4,000 km | 1.073 |
| Over 4,000 km | 1.048 |

Passenger kilometres are then passenger numbers times the detoured distance.

### 5. Emissions factors

Each route is assigned a distance band — domestic, short haul, or long haul —
from the destination country, using `countrycode` to resolve continent, with
Europe treated as short haul and a handful of territories assigned by hand.
Passenger kilometres are multiplied by a per-year factor from
`data/emissions_factors.xlsx`.

The factors are the UK government (DEFRA/DESNZ) conversion factors,
**including their radiative forcing uplift** for the non-CO₂ effects of
emissions released at altitude. The workbook's `Combined` sheet carries the
government's own with- and without-uplift figures side by side: in 2021 the
uplift was a factor of about 2.10 across all three bands. Pre-2018 years are
extended backwards using
[Larsson et al.](https://www.sciencedirect.com/science/article/pii/S0195925517303116),
whose method for measuring aviation emissions from a country's residents is the
closest published analogue to what this repository does.

**This is a conservative choice, and worth stating plainly.** Applying a factor
of roughly two follows UK government practice, but
[Lee et al. (2021)](https://doi.org/10.1016/j.atmosenv.2020.117834) put
aviation's total effective radiative forcing at about three times its CO₂ alone,
and a [CREDS briefing](https://www.creds.ac.uk/publications/the-non-co2-impacts-of-planes-are-a-key-reason-to-reduce-aviation-demand/)
argues that this higher figure is the one policy should use. The estimates here
are therefore more likely to understate aviation's warming effect than to
overstate it.

The workbook also holds well-to-tank factors, which are **not** currently added
to the published figures. Including them would raise emissions by a further 10%
or so.

---

## Outputs

`data/od_emissions_2024.gpkg` — a GeoPackage of great-circle lines, one per
airport pair, with:

| Field | Meaning |
| --- | --- |
| `airport1`, `airport1_country` | The UK end of the route |
| `airport2`, `airport2_country` | The other end |
| `length_km`, `length_km_detour` | Direct and detour-adjusted distance |
| `continent`, `country_region` | Destination groupings for charting |
| `distance_band` | `domestic`, `shorthaul`, or `longhaul` |
| `pass_1990` … `pass_2024` | Passengers per year |
| `flt_1990` … `flt_2024` | Flights per year |
| `pass_km_1990` … `pass_km_2024` | Passenger kilometres per year |
| `emissions_1990` … `emissions_2024` | kg CO₂e per year |

Intermediate files at each stage are kept in `data/` under names that record the
stage and the final data year.

---

## Limitations

**Residents and visitors are not separated.** The CAA counts passengers using UK
airports, not the nationality or residence of the people on board. A share of
these journeys belongs to inbound visitors rather than to UK residents. The
downstream allocation in `build` applies Great Britain's share of UK flights,
but this repository does not attempt to strip out non-residents.

**Connecting passengers.** Someone flying Manchester–Amsterdam–Singapore appears
in the data as a short-haul journey; the long-haul leg from Amsterdam is not a
UK departure and is not counted. This understates long-haul emissions from
regional airports and overstates the short-haul share.

**Private and non-commercial aviation** is only present to the extent that it
appears in the CAA statistics, and business jets are poorly covered.

**Cargo is out of scope.** Freight carried in the holds of passenger aircraft is
not separated out, so all of a flight's emissions are attributed to its
passengers.

**Airport name matching is imperfect.** Both CAA sources identify airports by
free-text name, and the join between them rests on normalising those names. The
scripts contain a long tail of hand corrections, which is a sign of how messy
this is rather than that it has been solved.

**Pandemic years.** 2020 and 2021 are not representative of anything. 2019 is
the last normal year, and the recovery since has been uneven between routes.

**Emissions factors change more than emissions do.** The DEFRA factors are
revised annually and the revisions are sometimes large — the 2023 factors rose
substantially against 2022 — so part of the year-to-year movement in the
emissions series reflects a change in the factor rather than a change in flying.
Passenger kilometres are the more stable series to look at for trends in
activity.

---

## Repository layout

| Path | Contents |
| --- | --- |
| `RScripts/flights_import.R` | Read CAA punctuality data into an OD flight table |
| `RScripts/flights_prepare.R` | Resolve direction, normalise airport names |
| `RScripts/passenger_import.R` | Read CAA airport passenger statistics |
| `RScripts/passenger_prepare.R` | Normalise and reshape passenger data |
| `RScripts/combine_flights_passengers.R` | Join to geocoded airports and build the network |
| `RScripts/geocode_missing_airports_2024.R` | Locate airports absent from the reference set |
| `RScripts/OD2linestring.R` | Great-circle geometry for each airport pair |
| `RScripts/add_emissions.R` | Detour factors, distance bands, emissions |
| `data/CAA_airport/`, `data/CAA_punctuality/` | Source statistics, by year |
| `data/emissions_factors.xlsx` | Factors by year and distance band, with sources |

Run the scripts in the order listed. `max_year` is set near the top of the
import scripts and must be updated when a new year of CAA data is added.

## Requirements

R, with `dplyr`, `tidyr`, `sf`, `stplanr`, `countrycode`, `readxl`, `furrr`, and
`stringi`. `add_emissions.R` uses `furrr` for parallel country lookups and sets
28 workers by default; reduce this on a smaller machine.

## Data sources

- [CAA airport statistics](https://www.caa.co.uk/data-and-analysis/uk-aviation-market/airports/uk-airport-data/)
  — passenger numbers by route.
- [CAA punctuality statistics](https://www.caa.co.uk/data-and-analysis/uk-aviation-market/flight-punctuality/)
  — flight counts by route.
- [UK government greenhouse gas conversion factors](https://www.gov.uk/government/collections/government-conversion-factors-for-company-reporting)
  — emissions factors from 2018.
- Larsson, J., Kamb, A., Nässén, J. and Åkerman, J. Measuring greenhouse gas
  emissions from international air travel of a country's residents:
  methodological development and application for Sweden. *Environmental Impact
  Assessment Review*.
  <https://www.sciencedirect.com/science/article/pii/S0195925517303116>
- Dobruszkes, F. and Peeters, D. (2019). The magnitude of detours faced by
  commercial flights: a global assessment. *Journal of Transport Geography*, 79.
  <https://www.sciencedirect.com/science/article/pii/S0966692318305544>

## Related repositories

- [build](https://github.com/PlaceBasedCarbonCalculator/build) — allocates these
  totals to neighbourhoods and produces the website data.

## Licence

Code is published under the GNU Affero General Public Licence v3.0; see
[LICENSE](LICENSE). Source statistics remain under the licences of their
publishers.

## Citation

Morgan, M. (2026). Carbon & Place: Data and tools to understand the spatial
variation in carbon footprints. *Environment and Planning B: Urban Analytics and
City Science*, 53(3), 538–554. <https://doi.org/10.1177/23998083251401613>

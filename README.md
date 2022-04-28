# LIMA

Shiny app for [LIMA](https://www.stadt-zuerich.ch/prd/de/index/statistik/publikationen-angebote/datenbanken-anwendungen/liegenschaftenpreise.html) tool (Abfragetool Liegenschaftenmarkt).

The data from the LIMA (LIegenschaftenMArkt) tool on the Website of [Statistik Stadt Zürich](https://www.stadt-zuerich.ch/prd/de/index/statistik.html) are based on notification changes of ownership from the land registry offices. All sales of developed land in the territory of the city of Zurich are taken into account. The data is obtained from the Open Data portal of the city of Zurich.

## Price series and grouping

Three different price series are available with the LIMA tool:

- Price per square meter of land area
- Price per square meter of land area minus insurance value (=approximate value for land price)
- Price per square meter of residential area for condominium purchaes

Three groups are displayed for each of the price series:

- Whole properties: sales of whole parcels (excluding co-ownership and condominium ownership)
- Condominium ownership
- All sales (whole properties, condominiums, co-ownership shares)

## Approximate value for land price

Since there is hardly any undeveloped land left in Zurich, the pure land price cannot be determined directly from the real estate transfer prices. For the sale of developed properties, the land price most closely corresponds to the sales price per square meter of land area minus the insurance value for entire properties (i.e. without including transactions in condominium and co-ownership).

The purpose of substracting the building insurance value from the purchase price is to determine the theoretical land value, as the residual of the total price minus the building value, effectively the market value of the land. However, it must be remembered that the insurance value does not correspond to the current structural value of the building, but to the cost of reconstruction (new value of an identical building and not the current value reduced by age depreciation).

Since the new value is higher than the current value and thus the deduction for the building value tends to be too high when calculating the residual value, the residual values shown are likely to tend to be lower than the effective land price. They are therefore only an approximation of the land price and should be interpreted with caution. Further information in the [disclaimer](https://www.stadt-zuerich.ch/prd/de/index/statistik/publikationen-angebote/datenbanken-anwendungen/liegenschaftenpreise/disclaimer.html).

## New building and zoning regulations (BZO) as of 2019

With the revision of the building and zoning regulations (BZO) in 2016, the BZO 1999 was replaced. In the change of ownership, the prices from 2019 refer to the BZO 2016. The prices up to 2018 refer to the BZO 1999.

While the various mixed zones (core zones, neighborhood preservation zones, center zones) in the BZO 2016 are still largely comparable with the BZO 1999, the designations of the residential zones changed fundamentally. In the revision, it was determined that an additional floor is permitted in the residential zones to compensate for the previously permitted basement. Thus, former W3 zones became new W4 zones, W4 became W5, and W5 became W6. This means that, for example, the W4 zone now includes areas that were designated W3 until 2018, but are comparable under building law. Or conversely, W3 before 2018 is not comparable to W3 after 2018, but to W4 after 2018. The presentation of the query results in query 1 of LIMA refers to this change.

By using the this application you agree to the [disclaimer](https://www.stadt-zuerich.ch/prd/de/index/statistik/publikationen-angebote/datenbanken-anwendungen/liegenschaftenpreise/disclaimer.html). 

All data is available as OGD: https://data.stadt-zuerich.ch/dataset?tags=lima

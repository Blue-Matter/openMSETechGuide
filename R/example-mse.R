library(MSEtool)

## ---- run-hist ----
myHist <- Simulate(SingleStockOM, silent = TRUE)

## ---- explore-hist-biomass ----
Biomass(myHist) |> head()

Biomass(myHist, byAge = TRUE, byArea = TRUE) |> head()

SBiomass(myHist) |> head()

## ---- explore-hist-catch ----
Landings(myHist) |> head()

Discards(myHist) |> head()


## ---- array-dimensions ----

L_Array <- Landings(myHist, df = FALSE)
D_Array <- Discards(myHist, df = FALSE)

dim(L_Array)
dim(D_Array) # all discards are 0 for every simulation

Discards(myHist) <- Extend(Discards(myHist, df = FALSE), nSim = nSim(myHist))

dim(Discards(myHist, df = FALSE))


## ---- forward-projections ----
myMSE <- Project(Hist = myHist, MPs = ExampleMPs(), silent = TRUE)

## ---- forward-projections2 ----
myMSE <- Project(Hist = myHist, MPs = ExampleMPs())

## ---- explore-mse-biomass ----
SBiomass(myMSE) |> head()

Landings(myMSE) |> tail()

## ---- explore-mse-results ----
SB_SB0(myMSE) |>
  dplyr::filter(Period == 'Projection') |>
  dplyr::group_by(MP) |>
  dplyr::summarise(Median  = median(Value),
                   Lower   = quantile(Value, 0.1),
                   Upper   = quantile(Value, 0.9),
                   .groups = 'drop'
                  )

## ---- fig-mse-biomass ----

PlotSBiomass(myMSE, relative = "B0", probs = c(0.1, 0.9))


## ---- pm-examples ----

sbsbmsy <- PM_SBSBMSY(myMSE)
ffmsy   <- PM_FFMSY(myMSE)

dplyr::bind_rows(
  Array2DF(sbsbmsy@Mean) |> dplyr::mutate(PM = 'P(SB > SBMSY)'),
  Array2DF(ffmsy@Mean)   |> dplyr::mutate(PM = 'P(F < FMSY)')
) |> dplyr::select('PM', 'Stock', 'MP', 'Value')

## ---- pm-yield ----

yield <- PM_Yield(myMSE)

Array2DF(yield@Stat) |>
  dplyr::group_by(Stock, MP) |>
  dplyr::summarise(Mean_Yield = mean(Value), .groups = 'drop')


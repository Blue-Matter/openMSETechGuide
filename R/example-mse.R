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
  dplyr::summarise(Median = median(Value),
                   Lower = quantile(Value, 0.1),
                   Upper = quantile(Value, 0.9),
                   .groups = 'drop'
                  )

## ---- fig-mse-biomass ----

SB_SB0(myMSE) |>
  dplyr::group_by(MP, Stock, Year, Period) |>
  dplyr::summarise(
    Med  = median(Value),
    Lo   = quantile(Value, 0.1),
    Hi   = quantile(Value, 0.9),
    .groups = "drop"
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = Year, colour = MP, fill = MP)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = Lo, ymax = Hi), alpha = 0.2,
                       colour = NA) +
  ggplot2::geom_line(ggplot2::aes(y = Med)) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = max(Year[Period == "Historical"])),
    linetype = "dashed", colour = "grey40"
  ) +
  ggplot2::labs(y = "Spawning Biomass", x = "Year",
                colour = "MP", fill = "MP") +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)),
                               limits = c(0, NA)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")



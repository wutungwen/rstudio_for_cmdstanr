abs_df <- readr::read_csv("Advertising Budget and Sales.csv") |>
  dplyr::rename(date_id = `...1`) |>
  dplyr::mutate(across(c(`TV Ad Budget ($)`, `Radio Ad Budget ($)`, `Newspaper Ad Budget ($)`, `Sales ($)`), 
                       ~ scales::rescale(.), .names = "{.col}_normalized"))


abs_data_list <- list(
  media_type = 3,
  time_type = nrow(abs_df),
  max_lag = 6,
  train_period = nrow(abs_df) - 20,
  media = abs_df |>
    dplyr::select(`TV Ad Budget ($)_normalized`, `Radio Ad Budget ($)_normalized`, `Newspaper Ad Budget ($)_normalized`) |>
    as.matrix() |>
    t(),
  outcome = abs_df$`Sales ($)_normalized`,
  outcome_min = min(abs_df$`Sales ($)`),
  outcome_max = max(abs_df$`Sales ($)`)
)

m_mmm_init <- cmdstanr::cmdstan_model("mmm.stan")

m_mmm_estimate <- m_mmm_init$variational(
  data = abs_data_list,
  iter = 100000
)

m_mmm_summary <- m_mmm_estimate$summary()


m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "predicted_")) |>
  dplyr::bind_cols(
    answer = abs_df$`Sales ($)`
  ) |>
  dplyr::mutate(
    status = ifelse(dplyr::row_number() <= nrow(abs_df) - 20, "学習", "検証")
  ) |>
  ggplot2::ggplot() + 
  ggplot2::geom_point(ggplot2::aes(x = 1:200, y = answer, color = status)) + 
  ggplot2::geom_line(ggplot2::aes(x = 1:200, y = mean), color = "blue") + 
  ggplot2::geom_ribbon(ggplot2::aes(x = 1:200, ymin = q5, ymax = q95), fill = ggplot2::alpha("blue", 0.3)) + 
  ggplot2::theme_gray(base_family = "HiraKakuPro-W3")

m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "predicted_")) |>
  dplyr::bind_cols(
    answer = abs_df$`Sales ($)`
  ) |>
  dplyr::mutate(
    status = ifelse(dplyr::row_number() <= nrow(abs_df) - 20, "学習", "検証")
  ) |>
  ggplot2::ggplot() + 
  ggplot2::geom_point(ggplot2::aes(x = 1:200, y = answer - mean, color = status)) + 
  ggplot2::geom_ribbon(ggplot2::aes(x = 1:200, ymin = answer - q5, ymax = answer - q95), fill = ggplot2::alpha("blue", 0.3)) + 
  ggplot2::theme_gray(base_family = "HiraKakuPro-W3")

m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "predicted_")) |>
  dplyr::bind_cols(
    answer = abs_df$`Sales ($)`
  ) |>
  dplyr::mutate(
    status = ifelse(dplyr::row_number() <= nrow(abs_df) - 20, "学習", "検証")
  ) |>
  ggplot2::ggplot() + 
  ggplot2::geom_point(ggplot2::aes(x = mean, y = answer, color = status)) + 
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ggplot2::theme_gray(base_family = "HiraKakuPro-W3")


m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "adweight")) |>
  dplyr::mutate(
    id = purrr::map(
      variable,
      \(x){
        stringr::str_split(x, "\\[|\\]|,")[[1]][2:3] |>
          as.numeric()
      }
    )
  ) |>
  tidyr::unnest_wider(id, names_sep = "_") |>
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x = id_2, y = mean, color = as.factor(id_1))) + 
  ggplot2::geom_ribbon(ggplot2::aes(x = id_2, ymin = q5, ymax = q95, fill = as.factor(id_1)),
                       alpha = 0.3)

m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "contribution")) |>
  dplyr::mutate(
    id = purrr::map(
      variable,
      \(x){
        stringr::str_split(x, "\\[|\\]|,")[[1]][2:3] |>
          as.numeric()
      }
    )
  ) |>
  tidyr::unnest_wider(id, names_sep = "_") |>
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x = id_2, y = mean, color = as.factor(id_1)))


m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "adstock_hi")) |>
  dplyr::mutate(
    id = purrr::map(
      variable,
      \(x){
        stringr::str_split(x, "\\[|\\]|,")[[1]][2:3] |>
          as.numeric()
      }
    )
  ) |>
  tidyr::unnest_wider(id, names_sep = "_") |>
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x = id_2, y = mean, color = as.factor(id_1)))

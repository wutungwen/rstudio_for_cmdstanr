data("fred_md", package = "BVAR")

fred_df <- fred_md |>
  tibble::tibble() |>
  dplyr::select(UNRATE, CPIAUCSL, FEDFUNDS, INVEST) |>
  dplyr::mutate(dplyr::across(c(UNRATE, CPIAUCSL, FEDFUNDS, INVEST), 
                              ~ (. - mean(.))/sd(.), .names = "{.col}_normalized")) |>
  
  dplyr::mutate(
    CPIAUCSL_normalized = CPIAUCSL_normalized - dplyr::lag(CPIAUCSL_normalized, 1),
    FEDFUNDS_normalized = FEDFUNDS_normalized - dplyr::lag(FEDFUNDS_normalized, 1),
    INVEST_normalized = INVEST_normalized - dplyr::lag(INVEST_normalized, 1),
  ) |>
  tidyr::drop_na()


fred_data_list <- list(
  media_type = 3,
  time_type = nrow(fred_df),
  P = 10,
  max_lag = 24,
  train_period = nrow(fred_df) - 4,
  media = fred_df |>
    dplyr::select(CPIAUCSL_normalized, FEDFUNDS_normalized, INVEST_normalized) |>
    as.matrix() |>
    t(),
  normalized_time = (1:nrow(fred_df) - mean(1:nrow(fred_df)))/sd(1:nrow(fred_df)),
  outcome = fred_df$UNRATE_normalized
)

m_mmm_init <- cmdstanr::cmdstan_model("mmm_dirichlet.stan")

m_mmm_estimate <- m_mmm_init$variational(
  seed = 12345,
  data = fred_data_list,
  iter = 10000
)

m_mmm_summary <- m_mmm_estimate$summary()


g_predict <- m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "predicted_outcome")) |>
  dplyr::mutate(
    id = purrr::map_int(
      variable,
      \(x){
        stringr::str_split(x, "\\[|\\]|,")[[1]][2] |>
          as.numeric()
      }
    )
  ) |>
  dplyr::left_join(
    tibble::tibble(
      date = as.Date(rownames(fred_md))
    ) |>
      dplyr::mutate(
        id = dplyr::row_number()
      ),
    by = "id"
  ) |>
  dplyr::bind_cols(
    answer = fred_df$UNRATE_normalized
  ) |>
  dplyr::mutate(
    status = ifelse(dplyr::row_number() <= nrow(fred_df) - 4, "学習", "検証")
  ) |>
  ggplot2::ggplot() + 
  ggplot2::geom_point(ggplot2::aes(x = date, y = answer, color = status)) + 
  ggplot2::geom_line(ggplot2::aes(x = date, y = mean), color = "blue") + 
  ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = q5, ymax = q95), fill = ggplot2::alpha("blue", 0.3)) + 
  ggplot2::scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  ggplot2::theme_gray(base_family = "HiraKakuPro-W3") + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + 
  ggplot2::labs(title = "アメリカ失業率予測") + 
  ggplot2::guides(color = "none")

g_trend <- m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "predicted_trend")) |>
  dplyr::mutate(
    id = purrr::map_int(
      variable,
      \(x){
        stringr::str_split(x, "\\[|\\]|,")[[1]][2] |>
          as.numeric()
      }
    )
  ) |>
  dplyr::left_join(
    tibble::tibble(
      date = as.Date(rownames(fred_md))
    ) |>
      dplyr::mutate(
        id = dplyr::row_number()
      ),
    by = "id"
  ) |>
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x = date, y = mean), color = "blue") + 
  ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = q5, ymax = q95), fill = ggplot2::alpha("blue", 0.3)) + 
  ggplot2::scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  ggplot2::theme_gray(base_family = "HiraKakuPro-W3") + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + 
  ggplot2::labs(title = "アメリカ失業率トレンド")

estimated_state <- m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "estimated_state")) |> 
  dplyr::pull(mean) |> 
  matrix(ncol = 10) 

used_states <- estimated_state |>
  nrow() |>
  seq_len() |>
  purrr::map_int(
    \(x){
      which.max(estimated_state[x,])
    }
  ) |>
  unique()

g_state <- m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "estimated_state")) |>
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
  dplyr::left_join(
    tibble::tibble(
      date = as.Date(rownames(fred_md))
    ) |>
      dplyr::mutate(
        id_1 = dplyr::row_number()
      ),
    by = "id_1"
  ) |>
  dplyr::filter(id_2 %in% used_states) |>
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x = date, y = mean, color = as.factor(id_2))) + 
  ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = q5, ymax = q95, fill = as.factor(id_2)),
                       alpha = 0.3) + 
  ggplot2::scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  ggplot2::theme_gray(base_family = "HiraKakuPro-W3") + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + 
  ggplot2::labs(title = "アメリカ失業率トレンド変化点検出") + 
  ggplot2::guides(color = "none", fill = "none")

g_contribution <- m_mmm_summary |> 
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
  dplyr::mutate(
    id_1 = dplyr::recode(as.factor(id_1),
                         "1" = "CPIAUCSL",
                         "2" = "FEDFUNDS",
                         "3" = "INVEST")
  ) |>
  dplyr::left_join(
    tibble::tibble(
      date = as.Date(rownames(fred_md))
    ) |>
      dplyr::mutate(
        id_2 = dplyr::row_number()
      ),
    by = "id_2"
  ) |>
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x = date, y = mean, color = as.factor(id_1))) + 
  ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = q5, ymax = q95, fill = as.factor(id_1)), alpha = 0.3) + 
  ggplot2::scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  ggplot2::theme_gray(base_family = "HiraKakuPro-W3") + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + 
  ggplot2::labs(title = "estimated contribution", color = "variable", fill = "variable")

gridExtra::grid.arrange(g_predict, g_trend, g_state)

m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "predicted_outcome")) |>
  dplyr::bind_cols(
    answer = fred_df$UNRATE_normalized
  ) |>
  dplyr::mutate(
    status = ifelse(dplyr::row_number() <= nrow(fred_df) - 4, "学習", "検証")
  ) |>
  ggplot2::ggplot() + 
  ggplot2::geom_point(ggplot2::aes(x = 1:nrow(fred_df), y = answer - mean, color = status)) + 
  ggplot2::geom_ribbon(ggplot2::aes(x = 1:nrow(fred_df), ymin = answer - q5, ymax = answer - q95), fill = ggplot2::alpha("blue", 0.3)) + 
  ggplot2::theme_gray(base_family = "HiraKakuPro-W3")

m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "predicted_outcome")) |>
  dplyr::bind_cols(
    answer = fred_df$UNRATE_normalized
  ) |>
  dplyr::mutate(
    status = ifelse(dplyr::row_number() <= nrow(fred_df) - 4, "学習", "検証")
  ) |>
  ggplot2::ggplot() + 
  ggplot2::geom_point(ggplot2::aes(x = mean, y = answer, color = status)) + 
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ggplot2::theme_gray(base_family = "HiraKakuPro-W3")


m_mmm_summary |> 
  dplyr::filter(stringr::str_detect(variable, "adweight_unnormalized")) |>
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
  dplyr::filter(stringr::str_detect(variable, "adstock")) |>
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
  dplyr::filter(stringr::str_detect(variable, "beta\\[")) |>
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
  ggplot2::geom_line(ggplot2::aes(x = id_1, y = mean, color = as.factor(id_2)))


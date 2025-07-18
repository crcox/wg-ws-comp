library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)

cdi <- readRDS("data/asd_na-osg-2025-05-20.rds")
meta <- readRDS("data/cdi-metadata.rds")
allrecords <- cdi |>
    distinct(subjectkey, interview_age, sex, group, nproduced) |>
    write_csv(file = "data/all-records-no-form-info.csv")

forms <- read_csv("data/GUIDS_forms.csv", col_types = "iciic") |>
    select(-1) |>
    group_by(subjectkey, nproduced, interview_age) |>
    group_split() |>
    map(~ {
        if (nrow(.x) > 1) {
            filter(.x, form == "WS")
        } else {
            .x
        }
    }) |>
    list_rbind()

asd <- allrecords |>
    filter(group == "ASD") |>
    left_join(forms)

asd |>
    ggplot(aes(x = nproduced)) +
    geom_histogram() +
    ylab("number of assessments") +
    facet_wrap(vars(form))

asd |>
    ggplot(aes(x = nproduced)) +
    geom_histogram(aes(y = after_stat(density))) +
    facet_wrap(vars(form))

ws <- read_csv("data/NDAR_CDI_WS_word_replacements.csv")
wg <- read_csv("data/NDAR_CDI_WG_word_replacements.csv")
z <- wg$word_WordBank1 %in% meta$word
mean(z)
wg$word_WordBank1[!z]

# WG has in and inside as separate items.
# WS has "inside/in" as a single item.
# A child who produces either "in" or "inside" as assessed by WG is understood
# to produce "inside/in" in terms of the WS word list.

# For this logical variable, specifically toggle "inside/in" to "true".
# Other minor reconciliations between forms are encoded in `meta$word` and
# `wg$word_WordBank1`, respectively.
meta$on_WG <- meta$word %in% wg$word_WordBank1
meta$on_WG[meta$word == "inside/in"] <- TRUE

asd_cdi <- cdi |>
    filter(group == "ASD") |>
    left_join(asd) |>
    left_join(select(meta, num_item_id, lexical_class, on_WG)) |>
    as_tibble()

asd_cdi_counts <- asd_cdi |>
    group_by(subjectkey, nproduced, sex, group, interview_age, form) |>
    summarize(
        nproduced_ws_total = sum(produced),
        nproduced_ws_noun  = sum(produced & lexical_class == "nouns"),
        nproduced_ws_verb  = sum(produced & lexical_class == "verbs"),
        nproduced_ws_adj   = sum(produced & lexical_class == "adjectives"),
        nproduced_ws_func  = sum(produced & lexical_class == "function_words"),
        nproduced_ws_other = sum(produced & lexical_class == "other"),
        nproduced_wg_total = sum(on_WG & produced),
        nproduced_wg_noun  = sum(on_WG & produced & lexical_class == "nouns"),
        nproduced_wg_verb  = sum(on_WG & produced & lexical_class == "verbs"),
        nproduced_wg_adj   = sum(on_WG & produced & lexical_class == "adjectives"),
        nproduced_wg_func  = sum(on_WG & produced & lexical_class == "function_words"),
        nproduced_wg_other = sum(on_WG & produced & lexical_class == "other")
    ) |>
    ungroup() |>
    select(-nproduced) |>
    pivot_longer(
        cols = starts_with("nproduced_"),
        names_to = c("wordset", "wordtype"),
        names_prefix = "nproduced_",
        names_sep = "_",
        values_to = "nproduced"
    ) |>
    mutate(
        wordtype = factor(
            wordtype,
            levels = c("noun", "verb", "adj", "func", "other", "total")
        )
    )

count_wordtype_wg <- meta |>
    filter(on_WG) |>
    rename(wordtype = lexical_class) |>
    count(wordtype)

count_wordtype_wg <- bind_rows(
    count_wordtype_wg,
    tibble(wordtype = "total", n = sum(count_wordtype_wg$n))
)

count_wordtype_ws <- meta |>
    rename(wordtype = lexical_class) |>
    count(wordtype)

count_wordtype_ws <- bind_rows(
    count_wordtype_ws,
    tibble(wordtype = "total", n = sum(count_wordtype_ws$n))
)

count_wordtype <- bind_rows(
    WG = count_wordtype_wg,
    WS = count_wordtype_ws,
    .id = "form"
) |>
    mutate(
        wordtype = factor(
            wordtype,
            c("nouns", "verbs", "adjectives", "function_words", "other", "total"),
            c("noun", "verb", "adj", "func", "other", "total")
        )
    )

count_wordtype_diff <- count_wordtype |>
    pivot_wider(id_cols = wordtype, names_from = form, values_from = n) |>
    mutate(wordtype_diff = WS-WG)

asd_cdi_counts |>
    filter(wordset == "ws") |>
    left_join(rename(count_wordtype, wordtype_max = n)) |>
    mutate(
        nproduced = nproduced / wordtype_max
    ) |>
    ggplot(aes(x = nproduced, color = wordtype, size = wordtype)) +
        geom_density() +
        scale_size_manual(values = c(total = 2, noun = .7, verb = .7, adj = .7, func = .7, other = .7)) +
        #stat_ecdf(geom = "step") +
        facet_wrap(vars(form), scales = "free_x") +
        theme_bw(base_size = 12)

asd_cdi_counts |>
    filter(wordset == "ws") |>
    left_join(rename(count_wordtype, wordtype_max = n)) |>
    mutate(nproduced = nproduced / wordtype_max) |>
    ggplot(aes(x = nproduced, fill = wordtype)) +
    geom_histogram() +
    #stat_ecdf(geom = "step") +
    facet_grid(wordtype ~ form, scales = "free_x") +
    theme_bw(base_size = 12) +
    theme(legend.position = "none")

asd_cdi_counts |>
    filter(wordset == "ws") |>
    left_join(rename(count_wordtype, wordtype_max = n)) |>
    mutate(nproduced = nproduced / wordtype_max) |>
    ggplot(aes(x = nproduced, fill = wordtype)) +
    geom_histogram(aes(y = after_stat(density))) +
    facet_grid(wordtype ~ form, scales = "free_x") +
    theme_bw(base_size = 12) +
    theme(legend.position = "none")

asd_cdi_counts |>
    filter(wordset == "ws") |>
    left_join(rename(count_wordtype, wordtype_max = n)) |>
    ggplot(aes(x = nproduced, fill = wordtype)) +
    geom_histogram() +
    #stat_ecdf(geom = "step") +
    facet_grid(wordtype ~ form, scales = "free_x") +
    theme_bw(base_size = 12) +
    theme(legend.position = "none")

asd_cdi_counts |>
    filter(wordset == "ws") |>
    left_join(rename(count_wordtype, wordtype_max = n)) |>
    ggplot(aes(x = nproduced, fill = wordtype)) +
    geom_histogram(aes(y = after_stat(density))) +
    facet_grid(wordtype ~ form, scales = "free_x") +
    theme_bw(base_size = 12) +
    theme(legend.position = "none")

n_wg <- count_wordtype |>
    filter(form == "WG", wordtype == "total") |>
    pull(n)


asd_cdi_counts |>
    pivot_wider(
        id_cols = c(subjectkey:form, wordtype),
        names_from = wordset,
        names_prefix = "n_",
        values_from = nproduced
    ) |>
    left_join(rename(count_wordtype, wordtype_max = n)) |>
    left_join(select(filter(count_wordtype, form == "WG"), wordtype, wordtype_max_wg = n)) |>
    filter(
        n_ws <= wordtype_max_wg,
        form == "WS"
    ) |>
    ggplot(aes(x = n_wg, y = n_ws, color = wordtype)) +
        geom_point(color = "darkgrey") +
        geom_smooth(method = "gam") +
        geom_abline(slope = 1, intercept = 0) +
        facet_wrap(vars(wordtype), scales = "free") +
        theme_bw() +
        theme(legend.position = "none")

asd_cdi_ws |>
    filter(nproduced_ws <= n_wg) |>
    ggplot(aes(x = nproduced_ws, y = nproduced_wg / nproduced_ws)) +
    geom_point() +
    geom_smooth() +
    xlim(c(0, n_wg)) +
    ylim(c(0, 1))


tmp <- asd_cdi_counts |>
    rename(n = nproduced, kde = kde_wg) |>
    pivot_wider(
        id_cols = c(subjectkey:form, wordtype),
        names_from = wordset,
        values_from = c(n, kde)
    ) |>
    left_join(rename(count_wordtype, wordtype_max = n)) |>
    left_join(select(filter(count_wordtype, form == "WG"), wordtype, wordtype_max_wg = n)) |>
    group_by(wordtype)

models <- tmp |>
    filter(
        n_ws <= wordtype_max_wg,
        form == "WS"
    ) |>
    group_split() |>
    map(~ mgcv::gam(n_ws ~ n_wg, data = .x))

names(models) <- tmp |> group_keys() |> pull(wordtype)

asd_cdi_counts_wg <- asd_cdi_counts |>
    filter(form == "WG", wordset == "wg") |>
    select(-wordset) |>
    group_by(wordtype) |>
    mutate(
        pred_wg_to_ws_raw = c(predict(models[[wordtype[1]]], tibble(n_wg = nproduced))),
        pred_wg_to_ws = if_else(pred_wg_to_ws_raw < nproduced, nproduced, pred_wg_to_ws_raw),
        pred_error = pred_wg_to_ws - nproduced
    ) |>
    left_join(select(count_wordtype_diff, wordtype, wordtype_diff, n_wg = WG)) |>
    group_by(wordtype) |>
    mutate(
        pred_error_rate = pred_error / wordtype_diff,
        pproduced = nproduced / n_wg
    ) |>
    ungroup()

asd_cdi_counts_wg |>
    left_join(select(count_wordtype_diff, wordtype, wordtype_diff)) |>
    group_by(wordtype) |>
    mutate(pred_error = pred_error / wordtype_diff) |>
    ggplot(aes(x = pred_error, color = wordtype, size = wordtype)) +
        geom_density() +
        scale_size_manual(values = c(total = 2, noun = .7, verb = .7, adj = .7, func = .7, other = .7)) +
        facet_wrap(vars(form)) +
        theme_bw() +
        xlab("Predicted Error Rate")

asd_cdi_counts_wg |>
    left_join(select(count_wordtype_diff, wordtype, wordtype_diff)) |>
    group_by(wordtype) |>
    mutate(pred_error = pred_error / wordtype_diff) |>
    ggplot(aes(x = pred_error, color = wordtype, size = wordtype)) +
        #geom_density() +
        stat_ecdf(geom = "step") +
        scale_size_manual(values = c(total = 2, noun = .7, verb = .7, adj = .7, func = .7, other = .7)) +
        scale_y_continuous(breaks = seq(0,1,.2)) +
        facet_wrap(vars(form)) +
        theme_bw() +
        xlab("Predicted Error Rate")

asd_cdi_counts_wg |>
    left_join(select(count_wordtype_diff, wordtype, wordtype_diff, n_wg = WG)) |>
    group_by(wordtype) |>
    mutate(
        pred_error_rate = pred_error / wordtype_diff,
        pproduced = nproduced / n_wg
    ) |>
    ggplot(aes(x = pproduced, y = pred_error_rate, color = wordtype)) +
        geom_point() +
        facet_wrap(vars(wordtype)) +
        theme_bw() +
        xlab("Proportion of Words Produced") +
        ylab("Predicted Error Rate")


kernal_density_estimate <- function(x, t, bw = bw.nrd0, h = bw(x)) {
    if (length(t) == 1) {
        mean(dnorm(t, mean = x, sd = h))
    } else {
        t <- rep(t, each = length(x))
        colMeans(matrix(dnorm(t, mean = x, sd = h), nrow = length(x)))
    }
}

tmp <- asd_cdi_counts |>
    filter(form == "WG") |>
    group_by(wordtype)

wg_counts <- tmp |>
    group_split() |>
    map(~ .x$nproduced)

names(wg_counts) <- group_keys(tmp) |> pull(wordtype)

asd_cdi_counts_wg <- asd_cdi_counts_wg |>
    group_by(wordtype) |>
    mutate(
        kde = kernal_density_estimate(pproduced, pproduced)
    ) |>
    ungroup()

asd_cdi_counts_wg |>
    ggplot(aes(x = pproduced, y = kde, color = pred_error_rate)) +
        geom_line(linewidth = 2, lineend = "round") +
        scale_color_binned(type = "viridis") +
        theme_bw() +
        xlab("Proportion of Words Produced") +
        facet_wrap(vars(wordtype))


sd_cdi_counts_wg |>
    group_by(wordtype) |>
    summarize()


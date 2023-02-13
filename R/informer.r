pkg.env <- rlang::env(parent = rlang::empty_env())

pkg_common_cfgpath_params <- function() {
    c(
        "@param cfg_full_path Provide the full path to the YAML configuration file. Defaults to NA.",
        "@param cfg_fn The file name for the YAML configuration file. Defaults to config.yml.",
        "@param cfg_path The file path to the YAML configuration file. Defaults to '.'."
    )
}
pkg_common_cfg_params <- function() {
    c(
        "@param cfg A YAML configuration file with `sql` section that includes `driver` `server`, `db`, and `schema_default`."
    )
}
pkg_common_reload_params <- function() {
    c(
        "@param reload Force a reload of the config."
    )
}

#' Load the YAML configuration file.
#'
#' Load the specified YAML configuration file. If no file is provided,
#' load "./config.yml".
#'
#' @eval pkg_common_cfgpath_params()
#' @eval pkg_common_reload_params()
#'
#' @importFrom rlang env_get env_poke
#' @importFrom glue glue
#' @importFrom yaml yaml.load_file
#' @importFrom fs path
#' @importFrom cli cli_alert_info cli_warn
#'
#' @return A configuration dataframe.
#' @export
#'
getCfg <- function( cfg_full_path=NA_character_,
                    cfg_fn=NA_character_,
                    cfg_path=NA_character_,
                    reload=FALSE ) {

    dflt_cfg_fn = "config.yml"
    dflt_cfg_path = "."

    cfg <- rlang::env_get(pkg.env, "cfg", default=NA, inherit=TRUE)
    #print(glue::glue("cfg={cfg}"))

    if (all(is.na(cfg)) || is.null(cfg) || reload) {
        # Use a cached version unless reload is specified.
        #
        # If a parameter was passed for cfg_full_path, cfg_fn, or cfg_path,
        # then use that, Otherwise, do the first from...
        #
        # 1. Look for the file name and path parts in the current folder.
        # 2. Check for environment variables CCDWR_CFG_FULL_PATH, CCDWR_CFG_PATH, CCDWR_CFG_FN.
        # 3. Check for ccdwr.cfg.full_path, ccdwr.cfg.path, ccdwr.cfg.fn options.
        # 4. Return NA
        # TODO: This needs to be fixed to reflect the above comment

        opt_cfg_full_path <- getOption("ccdwr.cfg.full_path", default=NA_character_)
        opt_cfg_fn <- getOption("ccdwr.cfg.fn", default=NA_character_)
        opt_cfg_path <- getOption("ccdwr.cfg.path", default=NA_character_)

        env_cfg_full_path <- Sys.getenv("CCDWR_CFG_FULL_PATH")
        env_cfg_fn <- Sys.getenv("CCDWR_CFG_FN")
        env_cfg_path <- Sys.getenv("CCDWR_CFG_PATH")

        if (is.na(cfg_full_path) && is.na(cfg_fn) && is.na(cfg_path)) {

            #print(glue::glue("WHATIS opt_cfg_full_path: {opt_cfg_full_path}"))
            #print(glue::glue("WHATIS env_cfg_full_path: {env_cfg_full_path}"))

            # 1. Look for the file name and path parts in the current folder.
            if (fs::file_exists(fs::path(dflt_cfg_path,dflt_cfg_fn))) {
                cfg_full_path <- fs::path(dflt_cfg_path,dflt_cfg_fn)
            } else {

                # 2. Check for environment variables CCDWR_CFG_FULL_PATH, CCDWR_CFG_PATH, CCDWR_CFG_FN.
                if (!is.na(env_cfg_full_path) && (env_cfg_full_path != "")) {
                    cfg_full_path = env_cfg_full_path
                } else if (!is.na(env_cfg_fn) || !is.na(env_cfg_path)) {
                    if (!is.na(env_cfg_fn) && (env_cfg_fn != "")) {
                        #print(glue::glue("Using environment CCDWR_CFG_FN [{env_cfg_fn}]"))
                        cfg_fn <- env_cfg_fn
                    } else {
                        #print(glue::glue("Using dflt_cfg_fn: {dflt_cfg_fn}"))
                        cfg_fn = dflt_cfg_fn
                    }

                    if (!is.na(env_cfg_path) && (env_cfg_path != "")) {
                        #print(glue::glue("Using environment CCDWR_CFG_PATH [{env_cfg_path}]"))
                        cfg_path <- env_cfg_path
                    } else {
                        #print(glue::glue("Using dflt_cfg_path: {dflt_cfg_path}"))
                        cfg_path <- dflt_cfg_path
                    }

                    cfg_full_path <- fs::path(cfg_path,cfg_fn)

                # 3. Check for ccdwr.cfg.full_path, ccdwr.cfg.path, ccdwr.cfg.fn options.
                } else if (!is.na(opt_cfg_full_path) && (opt_cfg_full_path != "")) {
                    #print(glue::glue("Using option ccdwr.cfg.full_path [{opt_cfg_full_path}]"))
                    cfg_full_path = opt_cfg_full_path
                } else if (!is.na(opt_cfg_fn) || !is.na(opt_cfg_path)) {
                    if (!is.na(opt_cfg_fn)) {
                        #print(glue::glue("Using option ccdwr.cfg.fn [{opt_cfg_fn}]"))
                        cfg_fn <- opt_cfg_fn
                    } else if (!is.na(env_cfg_fn) && (env_cfg_fn != "")) {
                        #print(glue::glue("Using environment CCDWR_CFG_FN [{env_cfg_fn}]"))
                        cfg_fn <- env_cfg_fn
                    } else {
                        #print(glue::glue("Using dflt_cfg_fn: {dflt_cfg_fn}"))
                        cfg_fn = dflt_cfg_fn
                    }
                    if (!is.na(opt_cfg_path)) {
                        #print(glue::glue("Using option ccdwr.cfg.path [{opt_cfg_path}]"))
                        cfg_path <- opt_cfg_path
                    } else if (!is.na(env_cfg_path) && (env_cfg_path != "")) {
                        #print(glue::glue("Using environment CCDWR_CFG_PATH [{env_cfg_path}]"))
                        cfg_path <- env_cfg_path
                    } else {
                        #print(glue::glue("Using dflt_cfg_path: {dflt_cfg_path}"))
                        cfg_path <- dflt_cfg_path
                    }

                    cfg_full_path <- fs::path(cfg_path,cfg_fn)
                }
            }
        } else {

            if (is.na(cfg_full_path)) {
                if (!is.na(cfg_fn)) {
                    #print(glue::glue("Using cfg_fn: {cfg_fn}"))
                } else if (!is.na(opt_cfg_fn)) {
                    #print(glue::glue("Using option ccdwr.cfg.fn [{opt_cfg_fn}]"))
                    cfg_fn <- opt_cfg_fn
                } else if (!is.na(env_cfg_fn) && (env_cfg_fn != "")) {
                    #print(glue::glue("Using environment CCDWR_CFG_FN [{env_cfg_fn}]"))
                    cfg_fn <- env_cfg_fn
                } else {
                    #print(glue::glue("Using dflt_cfg_fn: {dflt_cfg_fn}"))
                    cfg_fn = dflt_cfg_fn
                }
                if (!is.na(cfg_path)) {
                    #print(glue::glue("Using cfg_path: {cfg_path}"))
                } else if (!is.na(opt_cfg_path)) {
                    #print(glue::glue("Using option ccdwr.cfg.path [{opt_cfg_path}]"))
                    cfg_path <- opt_cfg_path
                } else if (!is.na(env_cfg_path) && (env_cfg_path != "")) {
                    #print(glue::glue("Using environment CCDWR_CFG_PATH [{env_cfg_path}]"))
                    cfg_path <- env_cfg_path
                } else {
                    #print(glue::glue("Using dflt_cfg_path: {dflt_cfg_path}"))
                    cfg_path <- dflt_cfg_path
                }

                cfg_fn <- dplyr::if_else(is.na(cfg_fn),dflt_cfg_fn,cfg_fn)
                cfg_path <- dplyr::if_else(is.na(cfg_path),dflt_cfg_path,cfg_path)

                #print(glue::glue("Using cfg_fn and cfg_path: {cfg_fn} and {cfg_path}"))

                cfg_full_path <- fs::path(cfg_path,cfg_fn)
            } else {
                #print(glue::glue("Using cfg_full_path: {cfg_full_path}"))
            }
        }

        cli::cli_alert_info(glue::glue("Loading configuration from [{cfg_full_path}]"))

        if (fs::file_exists(cfg_full_path)) {
            #print(glue::glue("DEBUG: Current dir = [{getwd()}]"))

            cfg_l <- yaml::yaml.load_file(cfg_full_path)

            # print(glue::glue("DEBUG: str(cfg_l) = [{str(cfg_l)}]"))
            # print(glue::glue("DEBUG: cfg_l = [{cfg_l}]"))
            # print(glue::glue("DEBUG: cfg_l$config          = [{cfg_l$config}]"))
            # print(glue::glue("DEBUG: typeof(cfg_l) = [{typeof(cfg_l)}]"))
            # print(glue::glue("DEBUG: cfg_l$config$location = [{cfg_l$config$location}]"))

            if ("config" %in% names(cfg_l)) {

                # print(glue::glue("DEBUG: cfg_l$config exists"))

                if ("location" %in% names(cfg_l$config)) {
                    if (cfg_l$config$location != "self") {
                        cfg_full_path <- fs::path(cfg_l$config$location,cfg_fn)
                        cli::cli_alert_info(glue::glue("Redirecting loading configuration from [{cfg_full_path}]"))
                    }
                } else {
                    if ("location" %in% names(cfg_l)) {
                        if (cfg_l$location != "self") {
                            cfg_full_path <- fs::path(cfg_l$location,cfg_fn)
                            cli::cli_alert_info(glue::glue("Redirecting loading configuration from [{cfg_full_path}]"))
                        }
                    } # else self assumed
                } # else self assumed

                if (fs::file_exists(cfg_full_path)) {
                    cfg <- yaml::yaml.load_file(cfg_full_path)
                    rlang::env_poke(pkg.env, "cfg_full_path", cfg_full_path)
                }
            } else {
                cfg = cfg_l
                rlang::env_poke(pkg.env, "cfg_full_path", cfg_full_path)
            }

            rlang::env_poke(pkg.env, "cfg", cfg)
            #assign("cfg", cfg, envir=pkg.env)
        } else {
            cli::cli_warn(glue::glue("Configuration not found [{cfg_full_path}]"))
        }
    } #else {
        #cfg <- rlang::env_get(pkg.env, "cfg", default=NA)
        #print(glue::glue("Using cached cfg: {cfg_full_path}"))
    #}
    getCfg <- cfg
}

#' Set a new value in the cached YAML configuration file.
#'
#' @param section The section for the new variable
#' @param variable The name of the new variable
#' @param value The value for the new variable
#' @eval pkg_common_cfg_params()
#' @eval pkg_common_cfgpath_params()
#' @eval pkg_common_reload_params()
#'
#' @return A configuration dataframe.
#'
#' @importFrom rlang env_get
#'
#' @export
#'
setCfg <- function( section, variable, value,
                    cfg=NA_character_, cfg_full_path=NA_character_,
                    cfg_fn=NA_character_, cfg_path=NA_character_,
                    reload=FALSE
                    ) {
    if (all(is.na(cfg)) || is.null(cfg)) {
        cfg <- rlang::env_get(pkg.env, "cfg", default=NA)

        if (all(is.na(cfg)) || is.null(cfg)) {
            cfg <- getCfg(cfg_full_path=cfg_full_path, cfg_fn=cfg_fn, cfg_path=cfg_path, reload=reload)
        }
    }

    t <- list(value)
    names(t) <- variable
    cfg[section] <- list(t)
    rlang::env_poke(pkg.env, "cfg", cfg)
}

#' Return a data from data warehouse Colleague tables.
#'
#' Return a data from of the IPEDS cohort data. Data will come either from the file ipeds_cohorts.csv of
#' from the CCDW_HIST SQL Server database.
#'
#' @param file The name of the Colleague file to return
#' @param schema Which schema should be used. Needed for non-Colleague tables.
#' @param version Specify which version to include. Default is for the latest data. Any other value will return the dated file.
#' @param from_file_path Specify path for file. This overrides the use of the database. Defaults to NA.
#' @eval pkg_common_cfg_params()
#' @eval pkg_common_cfgpath_params()
#' @eval pkg_common_reload_params()
#' @param sep The separator to use in the field names. Default is a '.' as in the original Colleague file.
#' @param ext The extention to use for files.
#'
#' @importFrom stringr str_c
#' @importFrom fs path
#' @importFrom rlang env_get env_poke
#' @importFrom magrittr `%<>%`
#' @importFrom odbc dbConnect odbc
#' @importFrom dplyr tbl filter
#' @importFrom dbplyr in_schema
#' @importFrom readr read_csv cols col_character
#' @importFrom cli cli_abort
#'
#' @return The requested data in a dataframe.
#'
#' @export
#'
getColleagueData <- function( file,
                              schema=NA_character_, version=NA_character_,
                              db=NA_character_,
                              from_file_path=NA_character_,
                              sep='.', ext="csv",
                              #cols=NA_character_,
                              #where="",
                              cfg=NA_character_, cfg_full_path=NA_character_,
                              cfg_fn=NA_character_, cfg_path=NA_character_,
                              reload=FALSE
                              ) {

    # This is to remove the package build error: no visible global function definition for ...
    CurrentFlag <- NULL

    if (all(is.na(cfg)) || is.null(cfg)) {
        cfg <- rlang::env_get(pkg.env, "cfg", default=NA)

        if (all(is.na(cfg)) || is.null(cfg)) {
            cfg <- getCfg(cfg_full_path=cfg_full_path, cfg_fn=cfg_fn, cfg_path=cfg_path, reload=reload)
        }
    }

    if ("data_source" %nin% names(cfg)) {
        if (!is.na(cfg$data_source$from_file_path)) {
            cfg$data_source$dbtype <- "file"
        } else {
            cfg$data_source$dbtype <- "ccdw"
        }
    } else if ("dbtype" %nin% names(cfg$data_source)) {
        if (!is.na(cfg$data_source$from_file_path)) {
            cfg$data_source$dbtype <- "file"
        } else {
            cfg$data_source$dbtype <- "ccdw"
        }
    }

    if ("from_file_path" %nin% names(cfg$data_source)) {
        cfg$data_source$from_file_path <- NA_character_
    }

    cfg_dbtype <- tolower(cfg$data_source$dbtype)

    if (!is.na(from_file_path)) {
        cfg_from_file_path = from_file_path
        cfg_dbtype <- "file"
    } else if (cfg_dbtype == "file") {
        cfg_from_file_path = cfg$data_source$from_file_path
    }

    if (is.na(schema)) {
        if (cfg_dbtype == "ccdw") {
            schema <- "history"
        }

        if ("sql" %in% names(cfg)) {
            if ("schema_default" %in% names(cfg$sql)) {
                schema <- cfg$sql$schema_default
            }
        }

        if (is.na(schema)) {
            error(str_glue("You must either pass schema as a parameter ",
                           "or define a schema_default value under sql in ",
                           "your configuration file"))
        }
    }

    if (cfg_dbtype != "file") {
        if (is.na(db)) {
            db <- cfg$sql$db
        }

        conn_str <- stringr::str_c( glue::glue("Driver={<<cfg$sql$driver>>}",
                                               .open = "<<", .close = ">>"),
                                    glue::glue("Server={<<cfg$sql$server>>}",
                                               .open = "<<", .close = ">>"),
                                    glue::glue("Database={<<db>>}",
                                               .open = "<<", .close = ">>"),
                                    "Trusted_Connection=Yes",
                                    "Description=Informer.r:getColleagueData()",
                                    sep=";"
        )

        ccdwconn <- odbc::dbConnect( odbc::odbc(), .connection_string=conn_str )

        df <- dplyr::tbl(ccdwconn, dbplyr::in_schema(schema, file) )

        if (cfg_dbtype == "ccdw") {
            if (version == "latest" & schema=="history") {
                df %<>% filter( CurrentFlag == "Y" )
            }
        }

    } else {
        # Try to find <file> in a folder named <schema>
        fp_files <- list.files(path=fs::path(cfg_from_file_path,schema),glue::glue("^{file}.*{ext}"))
        if (length(fp_files) > 0) {
            csvpath <- fs::path(cfg_from_file_path,schema)
            csvfile <- glue::glue("^{file}.*{ext}")
        } else {
            # ..., then look for a file named <schema><sep><file>.csv
            fp_files <- list.files(path=fs::path(cfg_from_file_path),glue::glue("^{schema}{sep}{file}.*{ext}"))
            if (length(fp_files) > 0) {
                csvpath <- cfg_from_file_path
                csvfile <- glue::glue("^{schema}{sep}{file}.*{ext}")
            } else {
                # ..., then look for a file named <file>.csv
                fp_files <- list.files(path=fs::path(cfg_from_file_path),glue::glue("^{file}.*{ext}"))
                if (length(fp_files) > 0) {
                    csvfile <- glue::glue("^{file}.*{ext}")
                } else {
                    csvfile <- NA_character_
                    cli_abort(glue::glue("ERROR: File not found: {file}"))
                }
            }
        }

        #if (!exists("show_col_types") && is.na(show_col_types)) {
        #    show_col_types = FALSE
        #}
        df <- merge_files(
                path=csvpath,
                pattern=csvfile
            )
    }

    # if (dplyr::coalesce(where,"") != "") {
    #     where_expr <- rlang::parse_expr(where)
    #     df %<>% filter(!!where)
    # }

    # if (!is.na(cols)) {
    #     warn <- 0
    #
    #     if (!is.vector(cols)) {
    #         warning("Parameter cols is not a vector and will be ignored")
    #         warn <- 1
    #     } else if (length(cols) > 1) {
    #         cols_str <- paste(cols,";")
    #     } else {
    #         cols_str <- replace(cols,",",";")
    #     }
    #
    #     if (!warn) {
    #         cols_expr <- rlang::parse_exprs(cols_str)
    #         df %<>% select(!!!cols_expr)
    #     }
    # }

    if (sep != '.') names(df) <- gsub("\\.", sep, names(df))

    getColleagueData <- df
}


debug_test <- function() {
    cfg <- getCfg()
}

#' Convert a collection of multi-valued columns into delimiter-separated columns.
#'
#' That is, it takes this:
#'
#' ID         Col1       Col2
#' -------    --------   -------
#' 001        A          1
#' 001        B          2
#' 001        C          3
#'
#' ...and by invoking it with mv_to_delim(df, keys=c('ID'), assoc=dataframe('GRP1' = c('Col1','Col2'))),
#' converts it to this:
#'
#' ID         Col1       Col2
#' -------    --------   -------
#' 001        A, B, C    1, 2, 3
#'
#' @param df The source dataframe.
#' @param keys A list of all key columns, used to uniquely identify each "row".
#' @param assoc A dictionary (dataframe) of associations of multi-valued columns. These columns are collapsed together as a group.
#' @param cols A list of independent multi-valued columns. These columns are collapsed individually.
#' @param delim The separator to use between  values. This defaults to a comma followed by a space.
#' @importFrom rlang env_get env_poke maybe_missing
#' @importFrom glue glue
#' @importFrom yaml yaml.load_file
#' @importFrom fs path
#'
# export
mv_to_delim <- function( df, keys, assoc, cols, delim ) {
    # Get all the columns in the original order to restore at the end

    # Get names of columns used in the associations

    # If keys not supplied, make an empty list
    loc_keys <- rlang::maybe_missing(keys,c())

    # If cols not supplied, make an empty list
    loc_cols <- rlang::maybe_missing(cols,c())

    # Get list of columns not used at all, and create a dataframe with those columns with the keys,
    #   removing all the rows where all the columns are NAs (these were created for the multi-valued columns)

    # Process the associations one by one and add them back into the result df

    # Process the columns that remain one by one and add them back into the result df

    # Return the result dataframe with the columns in the original order
}

mv_to_comma <- function( df, keys, assoc, cols ) {
    return(mv_to_delim(df, keys, assoc, cols, delim=", "))
}

# def delim_to_mv(
#     df: pd.DataFrame,
#     keys: List = None,  # type: ignore
#     cols: List = None,  # type: ignore
#     delim: str = ", ",
#     fill: bool = True,
# ) -> pd.DataFrame:
#     """
#     This converts a delimiter-separated column into a multi-valued column.
#
#     keys (list)        List of all key columns, used to uniquely identify each "row"
#     cols (list)        List of independent multi-valued columns. These columns
#                           are collapsed individually.
#     delim (str)        The separator to use between values. This defaults to a comma
#                           followed by a space.
#     fill (bool)        Keep column filled with duplicates (True) or replace with NaN (False)
#                           [NOT IMPLEMENTED YET]
#
#     That is, it takes this:
#
#     ID         Col1       Col2
#     -------    --------   -------
#     001        A, B, C    1, 2, 3
#
#     ...and by invoking it with delim_to_mv(df, keys=['ID'], cols=['Col1','Col2']),
#         converts it to this:
#
#     ID         Col1       Col2
#     -------    --------   -------
#     001        A          1
#     001        B          2
#     001        C          3
#
#     """
#
# if keys is None:
#     keys = []
#
# if cols is None:
#     cols = []
#
# # Get all the columns in the original order to restore at the end
# colnames = list(df.columns)
#
# # Get list of columns not used at all, and create a dataframe with those columns with the keys,
# #   removing all the rows where all the columns are NAs (these were created for the multi-valued columns)
# unused_cols = list(set(colnames) - set(keys) - set(cols))
# result_df = df.loc[:, keys + unused_cols]
#
# build_df = pd.DataFrame()
#
# for col in cols:
#     collst = [col]
#
# dfc = (
#     df.loc[:, keys + collst]
#     .set_index(keys)[collst]
#     .apply(lambda x: x.split(delim) if type(x) == str else x.str.split(delim))
#     .explode(collst)
#     # .str
#     # .strip()
#     .reset_index()
# )
#
# if build_df.empty:
#     build_df = dfc.copy()
# else:
#     build_df[collst] = dfc.loc[:, collst]
#
# result_df = pd.merge(result_df, build_df, on=keys, how="left")
#
# if not fill:
#     pass
#
# return result_df.loc[:, colnames]
#
#
# def commas_to_mv(
#     df: pd.DataFrame,
#     keys: List = None,  # type: ignore
#     cols: List = None,  # type: ignore
#     fill: bool = True,
# ) -> pd.DataFrame:
#     if keys is None:
#     keys = []
#
# if cols is None:
#     cols = []
#
# return delim_to_mv(df, keys=keys, cols=cols, delim=", ", fill=fill)
#
#
#
#

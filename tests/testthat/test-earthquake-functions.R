testthat::test_that(
  "eq_location_clean removes country and title cases location",
  {
    testthat::expect_equal(
      eq_location_clean(
        "JORDAN: BAB-A-DARAA,AL-KARAK"
      ),
      "Bab-A-Daraa,Al-Karak"
    )
  }
)


testthat::test_that(
  "eq_clean_data creates DATE and numeric coordinates",
  {
    test_data <- data.frame(
      YEAR = c(2000, 2001),
      MONTH = c(1, 2),
      DAY = c(15, 20),
      LATITUDE = c("10.5", "20.5"),
      LONGITUDE = c("75.5", "80.5"),
      LOCATION_NAME = c(
        "INDIA: TEST LOCATION",
        "INDIA: ANOTHER LOCATION"
      )
    )
    
    result <- eq_clean_data(test_data)
    
    testthat::expect_s3_class(
      result$DATE,
      "Date"
    )
    
    testthat::expect_true(
      is.numeric(result$LATITUDE)
    )
    
    testthat::expect_true(
      is.numeric(result$LONGITUDE)
    )
    
    testthat::expect_equal(
      result$DATE[1],
      as.Date("2000-01-15")
    )
    
    testthat::expect_equal(
      result$LOCATION_NAME[1],
      "Test Location"
    )
  }
)


testthat::test_that(
  "eq_create_label creates HTML labels",
  {
    test_data <- data.frame(
      LOCATION_NAME = "MEXICO: OAXACA",
      EQ_PRIMARY = "7.5",
      TOTAL_DEATHS = "29"
    )
    
    result <- eq_create_label(test_data)
    
    testthat::expect_equal(
      result,
      paste0(
        "<b>Location:</b> Oaxaca",
        "<br><b>Magnitude:</b> 7.5",
        "<br><b>Total deaths:</b> 29"
      )
    )
  }
)


testthat::test_that(
  "geom_timeline creates a timeline layer",
  {
    test_data <- data.frame(
      DATE = as.Date(
        c(
          "2000-01-01",
          "2001-01-01"
        )
      ),
      COUNTRY = c(
        "INDIA",
        "JAPAN"
      )
    )
    
    layer <- geom_timeline(
      ggplot2::aes(
        x = DATE,
        y = COUNTRY
      ),
      data = test_data
    )
    
    testthat::expect_s3_class(
      layer,
      "LayerInstance"
    )
    
    testthat::expect_equal(
      class(layer$geom)[1],
      "GeomTimeline"
    )
  }
)


testthat::test_that(
  "geom_timeline_label creates a timeline label layer",
  {
    test_data <- data.frame(
      DATE = as.Date(
        c(
          "2000-01-01",
          "2001-01-01"
        )
      ),
      COUNTRY = c(
        "INDIA",
        "JAPAN"
      ),
      LOCATION_NAME = c(
        "Test Location 1",
        "Test Location 2"
      ),
      EQ_PRIMARY = c(
        "5.5",
        "6.5"
      )
    )
    
    layer <- geom_timeline_label(
      ggplot2::aes(
        x = DATE,
        y = COUNTRY,
        label = LOCATION_NAME
      ),
      data = test_data,
      n_max = 1
    )
    
    testthat::expect_s3_class(
      layer,
      "LayerInstance"
    )
    
    testthat::expect_equal(
      class(layer$geom)[1],
      "GeomTimelineLabel"
    )
  }
)


testthat::test_that(
  "eq_map creates a leaflet map",
  {
    test_data <- data.frame(
      LATITUDE = c(10, 20),
      LONGITUDE = c(75, 80),
      EQ_PRIMARY = c("5.5", "6.5"),
      LOCATION_NAME = c(
        "Test Location 1",
        "Test Location 2"
      )
    )
    
    map <- eq_map(
      test_data,
      annot_col = "LOCATION_NAME"
    )
    
    testthat::expect_s3_class(
      map,
      "leaflet"
    )
  }
)
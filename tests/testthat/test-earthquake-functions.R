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

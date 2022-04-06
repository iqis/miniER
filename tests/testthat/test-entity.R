test_that("entity_type() registers in global env and workspace", {
  entity_type("person")
  expect_s3_class(workspace$entity_types$person,
                  "miniER.entity_type")
  expect_s3_class(globalenv()$person,
                  "miniER.entity_type")

})

test_that("entity outputs correct format", {
  entity_type("person")

  expect_s3_class(person("Siqi"),
                  "miniER.entity")

  expect_equal(person("Siqi",
                      "Iqis"),
               c("Siqi",
                 "Iqis"),
               ignore_attr = TRUE)
})

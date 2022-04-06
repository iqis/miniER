test_that("relationship type one_one() registers in global env and workspace", {

  entity_type("person")
  entity_type("location")

  "lives_in" %>% one_one("person", "location")

  expect_s3_class(workspace$entity_types$person,
                  "miniER.entity_type")
  expect_s3_class(globalenv()$person,
                  "miniER.entity_type")

  expect_s3_class(workspace$relationships$lives_in,
                  c("tibble",
                    "miniER.relationship_entry"))

  expect_setequal(colnames(workspace$relationships$lives_in),
                  c("person", "location"))

})


test_that("relationship type one_one() guards against undefined entity type ", {

  expect_error(
    "likes_to_eat" %>% one_one("pet", "food")
  )
})



test_that("relationship of relationship type one_one() stops on duplicate entry", {

  "person" %>% entity_type()
  "location" %>% entity_type()

  "lives_in" %>% one_one("person",
                         "location")

  person("Siqi") %>%
    lives_in(location("US"))

  expect_error(person("Siqi") %>%
                 lives_in(location("DE")))
})


test_that("relationship of relationship type 'one_many' registers correct entry", {
  entity_type("person")
  entity_type("food")

  "likes_to_eat" %>% one_many("person",
                              "food")

  person("Siqi") %>%
    likes_to_eat(food("ramen",
                      "tea",
                      "quesadilla"))

  # person("Siqi") %>%
  #   likes_to_eat(list(food"ramen"),
  #                     food("tea"),
  #                     food("quesadilla"))

  expect_equal(workspace$relationships$likes_to_eat,
               tibble::tibble(person = c("Siqi", "Siqi", "Siqi"),
                              food = c("ramen", "tea", "quesadilla")),
               ignore_attr = TRUE)
  })

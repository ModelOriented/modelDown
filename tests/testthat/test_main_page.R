context("Check main page generation")

test_that("object is encrypted and decrypted", {
  encrypted_object <- encrypt_object(iris, "pass")
  decrypted_object <- decrypt_object(encrypted_object, "pass")
  expect_equal(iris, decrypted_object)
})

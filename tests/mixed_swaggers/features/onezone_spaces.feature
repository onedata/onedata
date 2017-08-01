Feature: Onezone spaces
  Basic operations on spaces in onezone

  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser logged as user1 to Onezone service


  Scenario: User creates space using REST and sees in browser that new space has appeared
    When "user1" creates space "helloworld"
    Then user of browser sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel



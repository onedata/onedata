Feature: Onezone spaces
  Basic operations on spaces in onezone

  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser logged as user1 to Onezone service


  Scenario: User creates space using REST API and sees in browser that it has appeared
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user1 creates space "helloworld" in "z1" Onezone service
    And user of browser refreshes site
    And user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    Then user of browser sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

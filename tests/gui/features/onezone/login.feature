Feature: Onezone login page
  A site where you can login to Onezone.

  Background:
    Given user opened browser window
    And user of browser opened z1 onezone page
    # not used in non-homepage tests
    # And user clicks on the "login" link in Homepage main menu


  Scenario: Onezone login page renders with proper title
    Then user of browser should see that the page title contains "Login"


  Scenario: Login button of provider is rendered on the login Homepage page
    Then user of browser should see login button for username

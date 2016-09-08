Feature: Onezone login page
  A site where you can login to Onezone.

  Background:
    Given user opened browser window for browser
    And user of browser opens a Onezone URL
    # not used in non-homepage tests
    # And user clicks on the "login" link in Homepage main menu


  Scenario: Onezone login page renders with proper title
    Then user of browser should see that the page title contains "Login"


  Scenario: Login button of provider is rendered on the login Homepage page
    Then user of browser should see login button for plgrid


  Scenario: The development mode login page should show links to login
    When user of browser clicks on the "plgrid" login button
    Then user of browser should see a page with "Developer mode login:" header
    And user of browser should see [user1,user2,user3] links


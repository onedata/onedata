Feature: Onezone login page
  A site where you can login to Onezone.

  Background:
    Given user opens a Onezone URL in a web browser
    # not used in non-homepage tests
    # And user clicks on the "login" link in Homepage main menu


  Scenario: Onezone login page renders with proper title
    Then user should see that the page title contains "Login"


  Scenario: Login buttons of multiple providers are rendered on the login Homepage page
    Then user should see login buttons for [plgrid,dropbox,github,facebook,google]


  Scenario: The development mode login page should show links to login
    When user clicks on the "indigo" login button
    Then user should see a page with "Developer mode login:" header
    And user should see [user1,user2,user3] links


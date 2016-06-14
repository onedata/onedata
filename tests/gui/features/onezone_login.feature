Feature: Onezone login page
  A site where you can login to Onezone.

  Background:
    Given I'm visiting Onezone site


  # TODO: move this test to generic onezone tests for checking titles?
  Scenario: Onezone login page renders with proper title
    When I go to the /#/home/login relative URL
    Then The page title should contain "Login"


  Scenario: Rendering multiple login buttons
    When I go to the /#/home/login relative URL
    Then I should see at least 5 login buttons


  Scenario: Rendering particular login buttons
    When I go to the /#/home/login relative URL
    Then I should see login buttons for [plgrid,dropbox,github,facebook,google]


  # TODO: configure test to select the mode - currently all tests are in dev mode
  Scenario: Showing the development login list
    Given A login button for plgrid
    When I click on the login button
    Then I should be redirected to /dev_login page
    And I should see a development login page with at least 1 validate login link

  Scenario: Logging in with development login
    When I go to the /dev_login relative URL
    And I click on the first development login button
    Then I should be redirected to /onezone page

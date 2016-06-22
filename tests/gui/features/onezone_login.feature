Feature: Onezone login page
  A site where you can login to Onezone.

  Background:
    Given I'm visiting Onezone site


  Scenario: Onezone login page renders with proper title
    When I go to the /home/login Ember path
    Then The page title should contain "Login"


  Scenario: Rendering particular login buttons
    When I go to the /home/login Ember path
    Then I should see login buttons for [plgrid,dropbox,github,facebook,google]


  Scenario: Showing the development login list
    Given A login button for plgrid
    When I click on the login button
    Then I should be redirected to /dev_login page
    And I should see a development login page with at least 1 validate login link

  Scenario: Logging in with development login
    When I go to the /dev_login relative URL
    And I click on the first development login button
    Then I should be redirected to /onezone page

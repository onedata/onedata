Feature: Onezone login page
  A site where you can login to Onezone.

  Background:
    Given I'm visiting Onezone site


  # TODO: move this test to generic onezone tests for checking titles?
  Scenario Outline: Onezone page renders with proper title
    When I go to the <page> page
    Then The page title should contain <title>

    Examples:
    | page       | title |
    | home/login | login |


  Scenario: Rendering multiple login buttons
   When I go to the home/login page
   Then I should see at least 5 login buttons


  Scenario Outline: Rendering particular login buttons
   When I go to the home/login page
   Then I should see a <provider_name> login button

   Examples:
   | provider_name |
   | plgrid        |
  # # TODO: There is a problem or bug, because when using multiple examples, fixtures with scope module are invoked
  # # multiple times
#   | dropbox       |
#   | github        |
#   | google        |

Feature: Onezone GUI elements
  A user interface for managing Onezone account

  Background:
    Given I'm visiting Onezone site
    And I'm logged in to Onezone

  # We assume, that current alias is not "hello_alias"
  # the workaround is to use scenario outlines with at least two aliases

  Scenario Outline: User can change his alias using valid alias string
    When I uncollapse alias main accordion
    And I click on the user alias edit element
    And I type "<name>" into active element
    And I press enter on active element
    Then User alias should be changed to "<name>"

    # TODO: scenario outline currently does not work with persistent_environment scope: https://jira.plgrid.pl/jira/browse/VFS-2206
    Examples:
    | name       |
    | helloworld |

Feature: Onezone login page
  A site where you can login to Onezone.

  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And user opened browser window
    And user of browser opened z1 onezone page


  Scenario: Onezone login page renders with proper title
    Then user of browser should see that the page title contains "Onezone - Login"


  Scenario: User successfully logout
    Given user of browser logged as user1 to Onezone service
    When user of browser expands account settings dropdown in "ACCOUNT MANAGE" Onezone top bar
    And user of browser clicks on LOGOUT item in expanded settings dropdown in "ACCOUNT MANAGE" Onezone top bar
    Then user of browser sees that URL matches: https?://[^/]*/#/home/login
    And user of browser should see that the page title contains "Onezone - Login"

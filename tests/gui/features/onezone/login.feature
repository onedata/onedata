Feature: Onezone login page
  A site where you can login to Onezone.

  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And user opened browser window
    And user of browser opened z1 onezone page


  Scenario: Onezone login page renders with proper title
    Then user of browser should see that the page title contains "Login"


  Scenario: User successfully logouts
    # login
    Given user of browser entered credentials of user1 in login form in oz login page
    And user of browser clicked on the Sign in button in oz login page

    # logout
    When user of browser expands account settings dropdown in "ACCOUNT MANAGE" Onezone top bar
    And user of browser clicks on LOGOUT item in expanded settings dropdown in "ACCOUNT MANAGE" Onezone top bar
    Then user of browser sees that URL matches: https?://[^/]*/#/home/login
    And user of browser should see that the page title contains "Login"

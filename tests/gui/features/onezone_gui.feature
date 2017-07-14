Feature: Onezone GUI elements
  A user interface for managing Onezone account


  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open [http://a.com, http://b.com] in [Firefox, Chrome]
    Given user opened browser window
    And user of browser opened Onezone URL
    # not used in non-homepage tests
    And user of browser clicked on the "username" login button
    And user of browser seen that "Login with username and password" modal has appeared
    And user of browser entered credentials of user1 in "Login with username and password" modal
    And user of browser clicked "Sign In" confirmation button in displayed modal


  Scenario: User successfully logout
    When user of browser expands account settings dropdown in "ACCOUNT MANAGE" Onezone top bar
    And user of browser clicks on LOGOUT item in expanded settings dropdown in "ACCOUNT MANAGE" Onezone top bar
    Then user of browser sees that URL matches: https?://[^/]*/#/home/login
    And user of browser should see that the page title contains "Login"

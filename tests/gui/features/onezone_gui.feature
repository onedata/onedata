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


  Scenario: User sees that home space of provider should have "cloud with home" icon
    When user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser sees that there is provider named "p1" in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser expands submenu of provider named "p1" by clicking on cloud in provider record in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that spaces counter for "p1" match number of displayed supported spaces in expanded submenu of given provider in expanded "GO TO YOUR FILES" Onezone panel
    Then user of browser sees that space named "space1" in submenu of provider named "p1" in expanded "GO TO YOUR FILES" Onezone panel is set as home

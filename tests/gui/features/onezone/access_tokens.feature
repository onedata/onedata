Feature: Management of access tokens in Onezone GUI


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser logged as user1 to Onezone service


  Scenario: User successfully creates access token
    When user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser clicks on "Create new access token" button in expanded "ACCESS TOKENS" Onezone panel
    Then user of browser sees exactly 1 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel


  Scenario: User successfully removes access token
    When user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser clicks on "Create new access token" button in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 1 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel


  Scenario: User successfully copies access token
    When user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser clicks on "Create new access token" button in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 1 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser clicks on copy icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees that token for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel has been copied correctly

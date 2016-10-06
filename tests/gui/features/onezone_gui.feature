Feature: Onezone GUI elements
  A user interface for managing Onezone account


  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open [http://a.com, http://b.com] in [Firefox, Chrome]
    Given user opened browser window
    And user of browser opened Onezone URL
    # not used in non-homepage tests
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user1" link


#  Scenario: User successfully changes his alias (presses ENTER after entering text)
#    When user of browser expands the "USER ALIAS" Onezone sidebar panel
#    And user of browser records his current alias
#    And user of browser clicks on the user alias in expanded "USER ALIAS" panel
#    And user of browser types "helloworld" on keyboard
#    And user of browser presses enter on keyboard
#    Then user of browser sees that the user alias is "helloworld"
#    # TODO remove after integrate with swagger
#    And user of browser clicks on the user alias in expanded "USER ALIAS" panel
#    And user of browser types recorded alias on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the user alias is as recorded one
#
#
#  Scenario: User successfully changes his alias (clicks on CONFIRM button after entering text)
#    When user of browser expands the "USER ALIAS" Onezone sidebar panel
#    And user of browser records his current alias
#    And user of browser clicks on the user alias in expanded "USER ALIAS" panel
#    And user of browser types "helloworld" on keyboard
#    And user of browser clicks on confirm button displayed next to user alias edit box
#    Then user of browser sees that the user alias is "helloworld"
#    # TODO remove after integrate with swagger
#    And user of browser clicks on the user alias in expanded "USER ALIAS" panel
#    And user of browser types recorded alias on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the user alias is as recorded one
#
#
#  Scenario: User sees that his alias remains unchanged after resigning from renaming it
#    When user of browser expands the "USER ALIAS" Onezone sidebar panel
#    And user of browser records his current alias
#    And user of browser clicks on the user alias in expanded "USER ALIAS" panel
#    And user of browser types "helloworld" on keyboard
#    And user of browser clicks on cancel button displayed next to user alias edit box
#    Then user of browser sees that the user alias is as recorded one
#
#
#  Scenario: User successfully creates access token
#    When user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
#    And user of browser sees exactly 0 items on tokens list in expanded "ACCESS TOKENS" panel
#    And user of browser clicks on "Create new access token" button in expanded "ACCESS TOKENS" panel
#    Then user of browser sees exactly 1 item on tokens list in expanded "ACCESS TOKENS" panel
#    # TODO remove after integrate with swagger
#    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" panel
#    And user of browser sees exactly 0 items on tokens list in expanded "ACCESS TOKENS" panel
#
#
#  Scenario: User successfully removes access token
#    # TODO remove after integrate with swagger
#    When user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
#    And user of browser sees exactly 0 items on tokens list in expanded "ACCESS TOKENS" panel
#    And user of browser clicks on "Create new access token" button in expanded "ACCESS TOKENS" panel
#
#    And user of browser sees exactly 1 item on tokens list in expanded "ACCESS TOKENS" panel
#    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" panel
#    And user of browser sees exactly 0 items on tokens list in expanded "ACCESS TOKENS" panel
#
#
#  Scenario: User successfully copies access token
#    # TODO remove after integrate with swagger
#    When user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
#    And user of browser sees exactly 0 items on tokens list in expanded "ACCESS TOKENS" panel
#    And user of browser clicks on "Create new access token" button in expanded "ACCESS TOKENS" panel
#
#    And user of browser sees exactly 1 item on tokens list in expanded "ACCESS TOKENS" panel
#    And user of browser clicks on copy icon for 1st item on tokens list in expanded "ACCESS TOKENS" panel
#    And user of browser sees that token for 1st item on tokens list in expanded "ACCESS TOKENS" panel has been copied correctly
#    # TODO remove after integrate with swagger
#    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" panel
#    And user of browser sees exactly 0 items on tokens list in expanded "ACCESS TOKENS" panel



  Scenario: User successfully creates space
    Given user of browser generates valid name string
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser clicks on "Create new access token" button in expanded "DATA SPACE MANAGEMENT" panel

    And user of browser clicks on the input box in "Data space management" sidebar panel

    And user of browser types given name on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees that new item has appeared on space list in expanded "DATA SPACE MANAGEMENT" Onezone panel

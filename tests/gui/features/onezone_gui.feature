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
#  Scenario: User sees that his alias remains unchanged after resigning from renaming it (clicks CANCEL button after entering alias)
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
#
#
#  Scenario: User successfully creates space (presses ENTER after entering text)
#    Given user of browser generates valid name string
#    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" panel
#    And user of browser clicks on input box next to create space icon in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser types given name on keyboard
#    And user of browser presses enter on keyboard
#    Then user of browser sees that new item has appeared on space list in expanded "DATA SPACE MANAGEMENT" Onezone panel
#
#
#  Scenario: User successfully creates space (clicks on CONFIRM button after entering text)
#    Given user of browser generates valid name string
#    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" panel
#    And user of browser clicks on input box next to create space icon in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser types given name on keyboard
#    And user of browser clicks on confirm button for input box next to create space icon in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    Then user of browser sees that new item has appeared on space list in expanded "DATA SPACE MANAGEMENT" Onezone panel
#
#
#  Scenario: User sees that no space has been created after resigning from creating it (clicks CANCEL button after entering space name)
#    Given user of browser generates valid name string
#    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" panel
#    And user of browser clicks on input box next to create space icon in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser types given name on keyboard
#    And user of browser clicks on cancel button for input box next to create space icon in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    Then user of browser does not see new item on space list in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User succesfully set space as home space
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that item named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel is marked as home space
    And user of browser clicks on home outline icon in item row for item named "space2" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that item named "space2" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel is marked as home space
    And user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser clicks on item named "p1" in providers list in expanded "GO TO YOUR FILES" Onezone panel

    And user of browser clicks on the "Go to your files" button in provider popup
    And user of browser sees that Oneprovider session has started

    # one must wait because gui is to slow to immediately change home space in op when it was changed in oz
    And user of browser is idle for 3 seconds
    And user of browser refreshes site
    And user of browser sees that displayed directory tree in sidebar panel belongs to home space named "space2"
    And user of browser clicks on the "spaces" tab in main menu sidebar
    And user of browser sees that home space icon is displayed next to name of space "space2" in spaces list

    # TODO rm after ategrating with swagger
    Then user of browser clicked on the "providers" tab in main menu sidebar
    And user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser click on home outline icon in item row for item named "space1" from spaces list in expanded "DATA SPACE MANAGEMENT" panel Onezone panel
    And user of browser sees that item named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel is marked as home space

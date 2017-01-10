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


  Scenario: User successfully changes his alias (presses ENTER after entering text)
    When user of browser expands the "USER ALIAS" Onezone sidebar panel
    And user of browser records his current alias displayed in "USER ALIAS" Onezone panel
    And user of browser activates edit box by clicking on the user alias in expanded "USER ALIAS" Onezone panel
    And user of browser types "helloworld" to active edit box
    And user of browser presses enter on keyboard
    Then user of browser sees that the user alias displayed in "USER ALIAS" Onezone panel is "helloworld"
    # TODO remove after integrate with swagger
    And user of browser activates edit box by clicking on the user alias in expanded "USER ALIAS" Onezone panel
    And user of browser types recorded alias to user alias edit box in "USER ALIAS" Onezone panel
    And user of browser presses enter on keyboard
    And user of browser sees that the user alias displayed in "USER ALIAS" Onezone panel is as recorded one


  Scenario: User successfully changes his alias (clicks on CONFIRM button after entering text)
    When user of browser expands the "USER ALIAS" Onezone sidebar panel
    And user of browser records his current alias displayed in "USER ALIAS" Onezone panel
    And user of browser activates edit box by clicking on the user alias in expanded "USER ALIAS" Onezone panel
    And user of browser types "helloworld" to active edit box
    And user of browser clicks on confirm button displayed next to active edit box
    Then user of browser sees that the user alias displayed in "USER ALIAS" Onezone panel is "helloworld"
    # TODO remove after integrate with swagger
    And user of browser activates edit box by clicking on the user alias in expanded "USER ALIAS" Onezone panel
    And user of browser types recorded alias to user alias edit box in "USER ALIAS" Onezone panel
    And user of browser presses enter on keyboard
    And user of browser sees that the user alias displayed in "USER ALIAS" Onezone panel is as recorded one


  Scenario: User sees that his alias remains unchanged after resigning from renaming it (clicks CANCEL button after entering alias)
    When user of browser expands the "USER ALIAS" Onezone sidebar panel
    And user of browser records his current alias displayed in "USER ALIAS" Onezone panel
    And user of browser activates edit box by clicking on the user alias in expanded "USER ALIAS" Onezone panel
    And user of browser types "helloworld" to active edit box
    And user of browser clicks on cancel button displayed next to active edit box
    Then user of browser sees that the user alias displayed in "USER ALIAS" Onezone panel is as recorded one


  Scenario: User successfully creates access token
    When user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser clicks on "Create new access token" button in expanded "ACCESS TOKENS" Onezone panel
    Then user of browser sees exactly 1 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
    # TODO remove after integrate with swagger
    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel


  Scenario: User successfully removes access token
    # TODO remove after integrate with swagger
    When user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser clicks on "Create new access token" button in expanded "ACCESS TOKENS" Onezone panel

    And user of browser sees exactly 1 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel


  Scenario: User successfully copies access token
    # TODO remove after integrate with swagger
    When user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser clicks on "Create new access token" button in expanded "ACCESS TOKENS" Onezone panel

    And user of browser sees exactly 1 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser clicks on copy icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees that token for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel has been copied correctly
    # TODO remove after integrate with swagger
    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel


  Scenario: User successfully creates space (presses ENTER after entering text)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser types "helloworld" to active edit box
    And user of browser presses enter on keyboard
    Then user of browser sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel
    # TODO remove after integrate with swagger
    And user of browser expands settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "LEAVE" item in settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that space named "helloworld" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User successfully creates space (clicks on CONFIRM button after entering text)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser types "helloworld" to active edit box
    And user of browser clicks on confirm button displayed next to active edit box
    Then user of browser sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel
    # TODO remove after integrate with swagger
    And user of browser expands settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "LEAVE" item in settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that space named "helloworld" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User sees that no space has been created after resigning from creating it (clicks CANCEL button after entering space name)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser types "helloworld" to active edit box
    And user of browser clicks on cancel button displayed next to active edit box
    Then user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User successfully leaves space
    # TODO remove after integrate with swagger
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser types "helloworld" to active edit box
    And user of browser clicks on confirm button displayed next to active edit box
    Then user of browser sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    And user of browser expands settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "LEAVE" item in settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that space named "helloworld" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel



#  #TODO gui not working
#  Scenario: User successfully renames space
#    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser sees that there is space named "Small space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser expands settings dropdown for space named "Small space" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
#    And user of browser clicks on the "RENAME" item in settings dropdown for space named "Small space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser sees that "Rename a space" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "NewNameSpace" on keyboard
#    And user of browser clicks "OK" confirmation button in displayed modal
#    Then user of browser sees an info notify with text matching to: .*Small space.*renamed.*NewNameSpace.*
#    And user of browser sees that there is no space named "Small space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser sees that there is space named "NewNameSpace" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    # TODO remove after integrate with swagger
#    And user of browser expands settings dropdown for space named "NewNameSpace" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
#    And user of browser clicks on the "RENAME" item in settings dropdown for space named "NewNameSpace" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser sees that "Rename a space" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "Small space" on keyboard
#    And user of browser clicks "OK" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*NewNameSpace.*renamed.*Small space.*
#    And user of browser sees that there is no space named "NewNameSpace" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser sees that there is space named "Small space" in expanded "DATA SPACE MANAGEMENT" Onezone panel




  Scenario: User uncollapses space submenu and sees supporting providers list
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that list of supporting providers for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel contains only: "p1"


  Scenario: User uncollapses space submenu and sees that providers count match number of displayed supporting providers
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that providers counter for "space1" match number of displayed supporting providers in expanded submenu of given space in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User successfully receives support token for space
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Get support" button in submenu for "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that dropright with token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel has appeared
    And user of browser sees that dropright contains nonempty token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser copy token from dropright for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees an info notify with text matching to: .*copied.*
    And user of browser sees that copied token matches displayed one


  Scenario: User sees that each click on Get support button results in different token
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Get support" button in submenu for "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that dropright contains nonempty token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser copy token from dropright for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Get support" button in submenu for "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that dropright contains nonempty token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that copied token does not match displayed one






#  Scenario: User succesfully set space as home space
#    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser sees that item named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel is marked as home space
#    And user of browser clicks on home outline icon in item row for item named "space2" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser sees that item named "space2" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel is marked as home space
#    And user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
#    And user of browser clicks on item named "p1" in providers list in expanded "GO TO YOUR FILES" Onezone panel
#
#    And user of browser clicks on the "Go to your files" button in provider popup
#    And user of browser sees that Oneprovider session has started
#
#    # one must wait because gui is to slow to immediately change home space in op when it was changed in oz
#    And user of browser is idle for 5 seconds
#    And user of browser refreshes site
#    And user of browser sees that displayed directory tree in sidebar panel belongs to home space named "space2"
#    And user of browser clicks on the "spaces" tab in main menu sidebar
#    And user of browser sees that home space icon is displayed next to name of space "space2" in spaces list
#
#    # TODO rm after ategrating with swagger
#    Then user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
#    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space1"
#    And user of browser sees an info notify with text matching to: .*space1.*home.*
#    And user of browser sees that home space icon has appeared next to displayed name of space "space1" in spaces list
#
#
#  Scenario: User uncollapses space submenu and sees Get support button and provider p1
#    Given user of browser generates valid name string
#    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser clicks on item named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    Then user of browser sees that submenu for space named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel has been expanded
#    And user of browser sees that submenu for space named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel contains p1 supporting provider(s) and Get support button
#
#
#  Scenario: User gets support for given space
#    Given user of browser generates valid name string
#    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser clicks on item named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser sees that submenu for space named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel has been expanded
#    And user of browser clicks on Get support for space named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    Then user of browser sees that token popup for space named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel has appeared
#    And user of browser can copy visible token from popup for space named "space1" in spaces list in expanded "DATA SPACE MANAGEMENT" Onezone panel has appeared

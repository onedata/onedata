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


  Scenario: User successfully receives support token for space (clicking on Get support button in space's submenu)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Get support" button in submenu for "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that dropright with token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel has appeared
    And user of browser sees that dropright contains nonempty token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser copy token from dropright for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees an info notify with text matching to: .*copied.*
    And user of browser sees that copied token matches displayed one


  Scenario: User successfully receives support token for space (clicking on Get support button in space's settings dropdown)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "GET SUPPORT" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
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


  Scenario: User opens provider popup by clicking on supporting provider in space's submenu
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "p1" provider in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that provider popup for provider named "p1" has appeared on world map


  Scenario: User can not unsupport space without confirming understanding od data loss
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on unsupport space for provider named "p1" in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Unsupport space" modal has appeared
    And user of browser sees that "I understand the risk of data loss" option in modal is not selected
    Then user of browser sees that "Yes" item displayed in modal is disabled


  Scenario: User can unsupport space
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "Small space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "Small space" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on unsupport space for provider named "p1" in submenu of space named "Small space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Unsupport space" modal has appeared
    And user of browser selects "I understand the risk of data loss" option in displayed modal
    Then user of browser sees that "Yes" item displayed in modal is enabled
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that there is/are no supporting provider(s) named "p1" for space named "Small space" in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User sees that after unsupporting space number of supporting providers is decreased
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "Unsupportable space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "Unsupportable space" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that providers counter for space named "Unsupportable space" displays 1 in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on unsupport space for provider named "p1" in submenu of space named "Unsupportable space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Unsupport space" modal has appeared
    And user of browser selects "I understand the risk of data loss" option in displayed modal
    And user of browser sees that "Yes" item displayed in modal is enabled
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that there is/are no supporting provider(s) named "p1" for space named "Unsupportable space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that providers counter for space named "Unsupportable space" displays 0 in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User sees that after going to Oneprovider the home space is automatically loaded into view
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "A" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "space1" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of "A" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "p1" provider in submenu of space named "A" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that provider popup for provider named "p1" has appeared on world map
    And user of browser clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser sees that Oneprovider session has started
    Then user of browser sees that displayed directory tree in sidebar panel belongs to home space named "space1"


  Scenario: User succesfully set space as home space (clicks on home outline)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "space1" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sets space named "space2" as home by clicking on home outline in that space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "space2" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel

    And user of browser expands submenu of "space2" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "p1" provider in submenu of space named "space2" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that provider popup for provider named "p1" has appeared on world map
    And user of browser clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser sees that Oneprovider session has started

    # TODO rm when propagation time in gui became faster
    And user of browser is idle for 15 seconds
    And user of browser refreshes webapp
    And user of browser sees that displayed directory tree in sidebar panel belongs to home space named "space2"
    And user of browser clicks on the "spaces" tab in main menu sidebar
    And user of browser sees that home space icon is displayed next to name of space "space2" in spaces list

    # TODO rm after ategrating with swagger
    Then user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space1"
    And user of browser sees an info notify with text matching to: .*space1.*home.*
    And user of browser sees that home space icon has appeared next to displayed name of space "space1" in spaces list


  Scenario: User succesfully set space as home space (clicks on SET AS HOME in space settings)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "space1" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands settings dropdown for space named "space2" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space2" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "space2" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel

    And user of browser expands submenu of "space2" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "p1" provider in submenu of space named "space2" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that provider popup for provider named "p1" has appeared on world map
    And user of browser clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser sees that Oneprovider session has started

    # TODO rm when propagation time in gui became faster
    And user of browser is idle for 15 seconds
    And user of browser refreshes webapp
    And user of browser sees that displayed directory tree in sidebar panel belongs to home space named "space2"
    And user of browser clicks on the "spaces" tab in main menu sidebar
    And user of browser sees that home space icon is displayed next to name of space "space2" in spaces list

    # TODO rm after ategrating with swagger
    Then user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space1"
    And user of browser sees an info notify with text matching to: .*space1.*home.*
    And user of browser sees that home space icon has appeared next to displayed name of space "space1" in spaces list


  Scenario: User can go to Oneprovider by clicking on Go to yout files in provider's popup
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser expands submenu of "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "p1" provider in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that provider popup for provider named "p1" has appeared on world map
    And user of browser clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser sees that Oneprovider session has started
    Then user of browser sees that URL matches https?://[^/]*/#/onedata/data/Scenario: User uncollapses provider submenu and sees that spaces count match number of displayed supported spaces


  Scenario: User sees that after going to Oneprovider, without having any home space, the first one alphabetically is loaded into view
    # TODO after integration with swagger remove this setup
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser types "helloworld" to active edit box
    And user of browser clicks on confirm button displayed next to active edit box
    And user of browser sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sets space named "helloworld" as home by clicking on home outline in that space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "helloworld" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "LEAVE" item in settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that space named "helloworld" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel

    And user of browser expands submenu of "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "p1" provider in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that provider popup for provider named "p1" has appeared on world map
    And user of browser clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser sees that Oneprovider session has started

    # TODO rm when propagation time in gui became faster
    And user of browser is idle for 15 seconds
    And user of browser refreshes webapp
    Then user of browser sees that displayed directory tree in sidebar panel belongs to space named "A"

    # TODO rm after ategrating with swagger
    And user of browser clicks on the "spaces" tab in main menu sidebar
    And user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space1"
    And user of browser sees an info notify with text matching to: .*space1.*home.*
    And user of browser sees that home space icon has appeared next to displayed name of space "space1" in spaces list


  Scenario: User sees that after unsupporting space number displayed in space counter for given provider decreases
    When user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser sees that there is provider named "p1" in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that spaces counter for provider named "p1" displays 6 in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser expands submenu of "p1" by clicking on cloud in provider record in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that spaces counter for "p1" match number of displayed supported spaces in expanded submenu of given provider in expanded "GO TO YOUR FILES" Onezone panel

    And user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser expands settings dropdown for space named "B" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "LEAVE" item in settings dropdown for space named "B" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that space named "B" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel

    Then user of browser sees that spaces counter for provider named "p1" displays 5 in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser expands submenu of "p1" by clicking on cloud in provider record in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that spaces counter for "p1" match number of displayed supported spaces in expanded submenu of given provider in expanded "GO TO YOUR FILES" Onezone panel

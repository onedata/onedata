Feature: Onezone GUI elements featuring single space management


  Background:
    Given user opened browser window

    And user of browser has account in "z1" Onezone service
    And initial spaces configuration in "z1" Onezone service:
        space1:
            owner: browser
            providers:
                - p1:
                    storage: asd
                    size: 1000000

    And user of browser opened z1 onezone page
    And user of browser clicked on the "username" login button
    And user of browser seen that "Login with username and password" modal has appeared
    And user of browser entered his credentials in "Login with username and password" modal
    And user of browser clicked "Sign In" confirmation button in displayed modal


  Scenario: User successfully leaves space
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "LEAVE" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that space named "space1" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User successfully renames space
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameSpace" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    Then user of browser sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    And user of browser sees that there is no space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that there is space named "NewNameSpace" in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User uncollapses space submenu and sees supporting providers list
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that list of supporting providers for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel contains only: "p1"


  Scenario: User uncollapses space submenu and sees that providers count match number of displayed supporting providers
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that providers counter for "space1" match number of displayed supporting providers in expanded submenu of given space in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User successfully receives support token for space (clicking on Get support button in space's submenu)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Get support" button in submenu for "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that dropright with token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel has appeared
    And user of browser sees that dropright contains non-empty token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser copy token from dropright for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees an info notify with text matching to: .*copied.*
    And user of browser sees that copied token matches displayed one


  Scenario: User successfully receives support token for space (clicking on Get support button in space's settings dropdown)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "GET SUPPORT" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that dropright with token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel has appeared
    And user of browser sees that dropright contains non-empty token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser copy token from dropright for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees an info notify with text matching to: .*copied.*
    And user of browser sees that copied token matches displayed one


  Scenario: User sees that each click on Get support button results in different token
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Get support" button in submenu for "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that dropright contains non-empty token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser copy token from dropright for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Get support" button in submenu for "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that dropright contains non-empty token for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that copied token does not match displayed one


  Scenario: User can not unsupport space without confirming understanding of data loss
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on unsupport space for provider named "p1" in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Unsupport space" modal has appeared
    And user of browser sees that "I understand the risk of data loss" option in modal is not selected
    Then user of browser sees that "Yes" item displayed in modal is disabled


  Scenario: User sees that after unsupporting space, number of supporting providers is decreased
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that providers counter for space named "space1" displays 1 in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on unsupport space for provider named "p1" in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Unsupport space" modal has appeared
    And user of browser selects "I understand the risk of data loss" option in displayed modal
    And user of browser sees that "Yes" item displayed in modal is enabled
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that there is/are no supporting provider(s) named "p1" for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that providers counter for space named "space1" displays 0 in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User succesfully set space as home space (clicks on home outline)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sets space named "space1" as home by clicking on home outline in that space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "space1" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User succesfully set space as home space (clicks on SET AS HOME in space settings)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "space1" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel

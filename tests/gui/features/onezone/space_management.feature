Feature: Onezone GUI elements featuring single space management


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And initial spaces configuration in "z1" Onezone service:
        space1:
            owner: user1
            providers:
                - p1:
                    storage: onestorage
                    size: 1000000

    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser entered credentials of user1 in login form in oz login page
    And user of browser clicked on the Sign in button in oz login page


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

    # rename space
    And user of browser expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameSpace" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # assert space was renamed
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


  Scenario: User succesfully set space as home space (clicks on home outline)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sets space named "space1" as home by clicking on home outline in that space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "space1" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User succesfully set space as home space (clicks on SET AS HOME in space settings)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that space named "space1" is set as home space in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User can not unsupport space without confirming understanding of data loss
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on unsupport space for provider named "p1" in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Unsupport space" modal has appeared
    And user of browser sees that "I understand the risk of data loss" option in modal is not selected
    Then user of browser sees that "Yes" item displayed in modal is disabled


  Scenario: User can unsupport space
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # unsupport space
    And user of browser clicks on unsupport space for provider named "p1" in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Unsupport space" modal has appeared
    And user of browser selects "I understand the risk of data loss" option in displayed modal
    And user of browser sees that "Yes" item displayed in modal is enabled
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # confirm space was unsupported
    Then user of browser sees that there is/are no supporting provider(s) named "p1" for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User sees that after unsupporting space, number of supporting providers is decreased
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that providers counter for space named "space1" displays 1 in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # unsupport space
    And user of browser clicks on unsupport space for provider named "p1" in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Unsupport space" modal has appeared
    And user of browser selects "I understand the risk of data loss" option in displayed modal
    And user of browser sees that "Yes" item displayed in modal is enabled
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # confirm test results
    Then user of browser sees that there is/are no supporting provider(s) named "p1" for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that providers counter for space named "space1" displays 0 in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User successfully receives support token for space (clicking on Add storage button in space's submenu)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Add storage" button in submenu for "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # get support token
    Then user of browser sees that modal "Add storage" has appeared
    And user of browser sees non-empty token in "Add storage" modal
    And user of browser copies token from "Add storage" modal
    And user of browser sees an info notify with text matching to: .*copied.*
    And user of browser sees that copied token matches displayed one


  Scenario: User successfully receives support token for space (clicking on Add storage button in space's settings dropdown)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "ADD STORAGE" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # get support token
    Then user of browser sees that modal "Add storage" has appeared
    And user of browser sees non-empty token in "Add storage" modal
    And user of browser copies token from "Add storage" modal
    And user of browser sees an info notify with text matching to: .*copied.*
    And user of browser sees that copied token matches displayed one


  Scenario: User generates different support tokens in Add storage modal
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Add storage" button in submenu for "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # get support token
    And user of browser sees that modal "Add storage" has appeared
    And user of browser sees non-empty token in "Add storage" modal
    And user of browser copies token from "Add storage" modal
    And user of browser sees an info notify with text matching to: .*copied.*

    # generate another token
    And user of browser generate another token in "Add storage" modal
    And user of browser sees non-empty token in "Add storage" modal
    Then user of browser sees that copied token does not match displayed one

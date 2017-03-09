Feature: Oneprovider functionality using multiple providers

  Background:
    Given user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser logged as user1
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User create space in one provider and sees that it was created also in other provider
    When user of browser clicks on the "spaces" tab in main menu sidebar
    And user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "multiprov" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that "multiprov" has appeared on spaces sidebar list

    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "multiprov" in expanded "DATA SPACE MANAGEMENT" Onezone panel

    And user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser clicks on "p2" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicks on the "Go to your files" button in provider popup
    And user of browser clicks on the "spaces" tab in main menu sidebar
    And user of browser sees "multiprov" in spaces sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "multiprov" item on the spaces sidebar list
    And user of browser clicks on the "LEAVE SPACE" item in settings dropdown for space named "multiprov"
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*multiprov.*left
    And user of browser sees that the modal has disappeared
    And user of browser is idle for 4 seconds
    And user of browser sees that "multiprov" has disappeared from spaces sidebar list


  Scenario: User change name of space in one provider and sees that it was changed also in other provider
    When user of browser clicks on the "spaces" tab in main menu sidebar

    And user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "space1"
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameSpace" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    And user of browser sees that the modal has disappeared
    Then user of browser sees that space1 has disappeared from spaces sidebar list
    And user of browser sees that NewNameSpace has appeared on spaces sidebar list

    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "NewNameSpace" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that there is no space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel

    And user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser clicks on "p2" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicks on the "Go to your files" button in provider popup
    And user of browser clicks on the "spaces" tab in main menu sidebar
    And user of browser sees "NewNameSpace" in spaces sidebar list
    And user of browser does not see "space1" in spaces sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "NewNameSpace" item on the spaces sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "NewNameSpace"
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "space1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that NewNameSpace has disappeared from spaces sidebar list
    And user of browser sees that space1 has appeared on spaces sidebar list


  Scenario: User leave space in one provider and sees that it was leaved from also in other provider
    When user of browser clicks on the "spaces" tab in main menu sidebar

    And user of browser clicks on settings icon displayed for "spaceC" item on the spaces sidebar list
    And user of browser clicks on the "LEAVE SPACE" item in settings dropdown for space named "spaceC"
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    Then user of browser sees an info notify with text matching to: .*C.*left
    And user of browser sees that the modal has disappeared
    And user of browser is idle for 4 seconds
    And user of browser sees that "spaceC" has disappeared from spaces sidebar list

    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "spaceC" in expanded "DATA SPACE MANAGEMENT" Onezone panel

    And user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser clicks on "p2" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicks on the "Go to your files" button in provider popup
    And user of browser clicks on the "spaces" tab in main menu sidebar
    And user of browser does not see "spaceC" in spaces sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "spaceC" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that "spaceC" has appeared on spaces sidebar list

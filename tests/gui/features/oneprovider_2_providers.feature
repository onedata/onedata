Feature: Oneprovider functionality using multiple providers

  Background:
    Given user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "devLogin" login button
    And user of browser logged as user1
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User creates space in one provider and sees that it was created also in other provider
    When user of browser clicks on the "spaces" tab in main menu sidebar
    And user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "multiprov" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that "multiprov" has appeared on spaces sidebar list

    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is space named "multiprov" in expanded "DATA SPACE MANAGEMENT" Onezone panel

    And user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser clicks on "p2" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicks on the "Go to your files" button in provider popup
    And user of browser clicks on the "spaces" tab in main menu sidebar
    Then user of browser sees "multiprov" in spaces sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "multiprov" item on the spaces sidebar list
    And user of browser clicks on the "LEAVE SPACE" item in settings dropdown for space named "multiprov"
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*multiprov.*left
    And user of browser sees that the modal has disappeared
    And user of browser is idle for 4 seconds
    And user of browser sees that "multiprov" has disappeared from spaces sidebar list


  Scenario: User changes name of space in one provider and sees that it was changed also in other provider
    When user of browser clicks on the "spaces" tab in main menu sidebar

    And user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "space1"
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameSpace" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that space1 has disappeared from spaces sidebar list
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
    Then user of browser does not see "space1" in spaces sidebar list

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


  Scenario: User leaves space in one provider and sees that it was leaved from also in other provider
    When user of browser clicks on the "spaces" tab in main menu sidebar

    And user of browser clicks on settings icon displayed for "spaceC" item on the spaces sidebar list
    And user of browser clicks on the "LEAVE SPACE" item in settings dropdown for space named "spaceC"
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*C.*left
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
    Then user of browser does not see "spaceC" in spaces sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "spaceC" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that "spaceC" has appeared on spaces sidebar list


  Scenario: User creates group in one provider and sees that it was created also in other provider
    When user of browser clicks on the "groups" tab in main menu sidebar
    And user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "multiprov" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that "multiprov" has appeared on groups sidebar list

    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "GROUP MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is group named "multiprov" in expanded "GROUP MANAGEMENT" Onezone panel

    And user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser clicks on "p2" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicks on the "Go to your files" button in provider popup
    And user of browser clicks on the "groups" tab in main menu sidebar
    Then user of browser sees "multiprov" in groups sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "multiprov" item on the groups sidebar list
    And user of browser clicks on the "LEAVE THIS GROUP" item in settings dropdown for group named "multiprov"
    And user of browser sees that "Leave the group" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*multiprov.*left
    And user of browser sees that the modal has disappeared
    And user of browser is idle for 4 seconds
    And user of browser sees that "multiprov" has disappeared from groups sidebar list


  Scenario: User changes name of group in one provider and sees that it was changed also in other provider
    When user of browser clicks on the "groups" tab in main menu sidebar

    And user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for group named "group1"
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameGroup" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*group1.*renamed.*NewNameGroup.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that group1 has disappeared from groups sidebar list
    And user of browser sees that NewNameGroup has appeared on groups sidebar list

    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "GROUP MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is group named "NewNameGroup" in expanded "GROUP MANAGEMENT" Onezone panel
    And user of browser sees that there is no group named "group1" in expanded "GROUP MANAGEMENT" Onezone panel

    And user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser clicks on "p2" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicks on the "Go to your files" button in provider popup
    And user of browser clicks on the "groups" tab in main menu sidebar
    And user of browser sees "NewNameGroup" in groups sidebar list
    Then user of browser does not see "group1" in groups sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "NewNameGroup" item on the groups sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for group named "NewNameGroup"
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "group1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameGroup.*renamed.*group1.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that NewNameGroup has disappeared from groups sidebar list
    And user of browser sees that group1 has appeared on groups sidebar list


  Scenario: User leaves group in one provider and sees that it was leaved from also in other provider
    When user of browser clicks on the "groups" tab in main menu sidebar

    And user of browser clicks on settings icon displayed for "group3" item on the groups sidebar list
    And user of browser clicks on the "LEAVE THIS GROUP" item in settings dropdown for group named "group3"
    And user of browser sees that "Leave the group" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*group3.*left
    And user of browser sees that the modal has disappeared
    And user of browser is idle for 4 seconds
    And user of browser sees that "group3" has disappeared from groups sidebar list

    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "GROUP MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no group named "group3" in expanded "GROUP MANAGEMENT" Onezone panel

    And user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser clicks on "p2" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicks on the "Go to your files" button in provider popup
    And user of browser clicks on the "groups" tab in main menu sidebar
    Then user of browser does not see "group3" in groups sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "group3" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that "group3" has appeared on groups sidebar list

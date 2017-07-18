Feature: Basic data tab operations on single directory in file browser


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
            storage:
                defaults:
                    provider: p1
                directory tree:
                    - dir1:
                        - file1: 11111

    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser logged as user1 to Onezone service
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on "p1" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicked on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser seen that Oneprovider session has started


  Scenario: User fails to create new directory because of existing directory with given name
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees an error notify with text matching to: .*failed.*


  Scenario: User removes existing directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal

    Then user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User renames directory (presses ENTER after entering dir name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "new_dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    Then user of browser sees an info notify with text matching to: .*renamed.*
    And user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "new_dir1" has appeared in file browser
    And user of browser sees that item named "new_dir1" is directory in file browser


  Scenario: User renames directory (clicks OK confirmation button after entering dir name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "new_dir1" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    Then user of browser sees an info notify with text matching to: .*renamed.*
    And user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "new_dir1" has appeared in file browser
    And user of browser sees that item named "new_dir1" is directory in file browser


  Scenario: User sees that with only one directory selected only ["Create directory", "Create file", "Share element", "Edit metadata", "Upload file", "Rename element", "Change element permissions", "Remove element"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    Then user of browser sees that ["Create directory", "Create file", "Share element", "Edit metadata", "Upload file", "Rename element", "Change element permissions", "Remove element"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Copy element", "Cut element", "Show file distribution"] buttons are disabled in toolbar in data tab in Oneprovider gui

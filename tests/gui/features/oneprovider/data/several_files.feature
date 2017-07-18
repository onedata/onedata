Feature: Basic data tab operations on several files in file browser


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
                    - file1: 11111
                    - file2: 11111
                    - file3: 11111
                    - file4: 11111
                    - file5: 11111
                    - file6: 11111
                    - file7: 11111

    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser logged as user1 to Onezone service
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on "p1" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicked on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser seen that Oneprovider session has started


  Scenario: User selects a bunch of files using ctrl
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser sees item(s) named ["file1", "file2", "file3", "file4", "file5", "file6", "file7"] in file browser in given order
    And user of browser selects ["file3", "file1"] item(s) from file browser with pressed ctrl
    Then user of browser sees that ["file3", "file1"] items are selected in file browser
    And user of browser sees that "file2" item is not selected in file browser


  Scenario: User selects bunch of files using shift
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    And user of browser sees item(s) named ["file1", "file2", "file3", "file4", "file5", "file6", "file7"] in file browser in given order
    And user of browser selects ["file3", "file1"] item(s) from file browser with pressed shift
    Then user of browser sees that ["file3", "file2", "file1"] items are selected in file browser


  Scenario: User selects bunch of files using ctrl and shift
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser sees item(s) named ["file1", "file2", "file3", "file4", "file5", "file6", "file7"] in file browser in given order
    And user of browser selects ["file3", "file1"] item(s) from file browser with pressed shift
    And user of browser selects "file5" item(s) from file browser with pressed ctrl
    And user of browser selects "file7" item(s) from file browser with pressed shift
    Then user of browser sees that ["file7", "file6", "file5", "file3", "file2", "file1"] items are selected in file browser


  Scenario: User sees that with several files selected only ["Create directory", "Create file", "Upload file", "Change element permissions", "Remove element"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser selects ["file1", "file2"] item(s) from file browser with pressed ctrl
    Then user of browser sees that ["Create directory", "Create file", "Upload file", "Change element permissions", "Remove element"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Edit metadata", "Share element", "Rename element", "Copy element", "Cut element", "Show file distribution"] buttons are disabled in toolbar in data tab in Oneprovider gui


  Scenario: User removes several files
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser selects ["file1", "file2", "file3"] item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    Then user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["file1", "file2", "file3"] have disappeared from files browser

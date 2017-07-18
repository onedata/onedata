Feature: Basic data tab operations on nested directory structure in file browser


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
                    - dir1:
                        - file2: 11111
                        - file3: 11111
                    - dir2:
                        - dir3
                        - dir4:
                            - file4: 11111
                            - file5: 11111
                            - file6: 11111
                            - file7: 11111
                            - file8: 11111

    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser logged as user1 to Onezone service
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on "p1" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicked on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser seen that Oneprovider session has started


  Scenario: User changes directory using breadcrumbs
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir2
    And user of browser double clicks on item named "dir4" in file browser
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir2/dir4

    And user of browser changes current working directory to space1 using breadcrumbs
    Then user of browser sees that current working directory displayed in breadcrumbs is space1


  Scenario: User changes directory using sidebar directory tree view (with unfolding of folded directories)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser sees that current working directory displayed in directory tree is /
    And user of browser does not see /dir2/dir4/ in directory tree
    And user of browser changes current working directory to /dir2/dir4/ using directory tree
    Then user of browser sees that current working directory displayed in directory tree is /dir2/dir4/
    And user of browser sees item(s) named ["file4", "file5", "file6", "file7", "file8"] in file browser in given order


  Scenario: User sees that with several directories selected only ["Create directory", "Create file", "Upload file", "Change element permissions", "Remove element"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects ["dir1", "dir2"] item(s) from file browser with pressed ctrl
    Then user of browser sees that ["Create directory", "Create file", "Upload file", "Change element permissions", "Remove element"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Share element", "Edit metadata", "Rename element", "Copy element", "Cut element", "Show file distribution"] buttons are disabled in toolbar in data tab in Oneprovider gui


  Scenario: User sees that with directory and file selected only ["Create directory", "Create file", "Upload file", "Remove element"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects ["dir1", "file1"] item(s) from file browser with pressed ctrl
    Then user of browser sees that ["Create directory", "Create file", "Upload file", "Remove element"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Share element", "Edit metadata", "Rename element", "Change element permissions", "Copy element", "Cut element", "Show file distribution"] buttons are disabled in toolbar in data tab in Oneprovider gui


  Scenario: User removes directory containing several files
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal

    Then user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User enters directory and views files in it
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser double clicks on item named "dir1" in file browser
    Then user of browser sees item(s) named ["file2", "file3"] in file browser in given order


  Scenario: User opens nested directory via url and sees expanded directory tree in sidebar
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser changes current working directory to /dir2/dir4/ using directory tree
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir2/dir4
    And user of browser copies url from browser's location bar
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser refreshes site

    And user of browser does not see /dir2/dir4/ in directory tree
    And user of browser opens copied URL in browser's location bar
    Then user of browser sees that current working directory displayed in directory tree is /dir2/dir4/
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees item(s) named ["file4", "file5", "file6", "file7", "file8"] in file browser in given order

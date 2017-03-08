Feature: Oneprovider functionality using multiple providers

  Background:
    Given user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser logged as user1
    And user of browser created and recorded access token for later use with CDMI API
    And user of browser records providers hostname using copy hostname button in every provider popup
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User uploads a small file to space that accepts large files
    When user of browser uses spaces select to change data space to "space4"

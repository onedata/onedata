Feature: User management

  Background:
    Given openId server is started
    And u1 is logged in
    And u1 has macaroon

  Scenario: User registration
    When u1 registers in onedata
    And u1 is registered
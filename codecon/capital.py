#!/usr/bin/python3

import math

cap_gain = 0.15
tax = 0.33

income = 300e3
# Sevings we can use, 50-60k we leave as "safety net"
savings = 200e3

rent = 3000 * 12
expenses = 72e3 - rent

house = 1.5e6
house_down = savings
house_appr = 0.025
prop_tax = 0.01
interest = 0.04

investment = savings
investment_return = 0.10


# Returns list of payments if for of (interest, principal).
def loan_payments(principal, term, interest):
    p = math.pow(1 + interest, 1 / 12)
    q = math.pow(p, term)
    montly_payment = principal * (p - 1) * q / (q - 1)
    payments = []
    principal_left = principal
    for i in range(term):
        interest_payment = principal_left * (p - 1)
        payments.append((interest_payment, montly_payment - interest_payment))
        principal_left -= montly_payment - interest_payment
    return payments


def extend_to_year(payments):
    while len(payments) < 12:
        payments.append((0, 0))
    return payments


def sum_half_year(payments):
    si = 0
    sp = 0
    for (i, p) in payments[:6]:
        si += i
        sp += p
    return (si, sp)


def model_buy_house():
    principal = house - house_down
    term = 30 * 12
    months = 0
    cash = 0
    while True:
        # Model one year
        # TODO: Take into account FB stock growth
        # TODO: Capital gains on growth

        # Pay first half and then pay principal down:
        # print('{} months, principal {}'.format(months, principal))
        payments = extend_to_year(loan_payments(principal, term - months, interest))
        months += 6
        i1, p1 = sum_half_year(payments)
        left1 = 0.5 * (income * (1 - tax) - expenses) - (i1 + p1)
        principal -= p1 + left1
        if principal <= 0:
            cash = -principal
            break

        # Second half, get tax return and pay property tax
        # print('{} months, principal {}'.format(months, principal))
        payments = extend_to_year(loan_payments(principal, term - months, interest))
        months += 6
        i2, p2 = sum_half_year(payments)
        left2 = 0.5 * (income * (1 - tax) - expenses) - (i2 + p2)
        # Tax return and propery tax
        principal -= p2 + left2 + (i1 + i2) * tax - house * prop_tax
        if principal <= 0:
            cash = -principal
            break

    print('In {} months I have house+cash of {}'.format(
            months,
            house * math.pow(1 + house_appr, months / 12) + cash,
    ))
    return months


def term_return(term):
    return math.pow(math.pow(1 + investment_return, 1 / 12), term)


def model_invest(term):
    total = investment * term_return(term)
    invest_monthly = (income * (1 - tax) - (rent + expenses)) / 12
    for m in range(term):
        total += invest_monthly * term_return(term - m)
    invested = investment + term * invest_monthly
    print("Investment over {} months is {}".format(
        term,
        invested + (total - invested) * (1 - cap_gain),
    ))


print('''Parameters:
  income: {}
  house price: {}
  rent: {}
  return: {}
'''.format(int(income), int(house), int(rent / 12), int(investment_return * 100))
)

payoff_months = model_buy_house()
model_invest(payoff_months)

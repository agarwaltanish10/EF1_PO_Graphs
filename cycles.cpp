#include <bits/stdc++.h>
using namespace std;

// utilities
typedef long long ll;
typedef long double ld;

#define cout_vector(vec) for (ll i=0; i<vec.size(); i++){cout << vec[i] << ' ';} cout << '\n';
#define cout_matrix(mat) for (ll i=0; i<mat.size(); i++){for(ll j=0; j<mat[i].size(); j++){cout<<mat[i][j]<<' ';} cout << '\n';}

// terminal output colors
#define RESET "\033[0m"
#define GREEN "\033[32m"
#define BLUE  "\033[34m"

// constants
const ld PRICE_EPSILON = 1e-5;
const ld MBB_EPSILON = 1e-50;
const ll MAX_VAL = 200; // range of values in valuation matrix (v_max <= MAX_VAL), used in random generation of val matrix


class EF1POAllocation
{
    private:
        const ll n, m;                        // n agents, m goods
        const vector<vector<ll>> vals;        // valuation matrix: n x m
        const set<ll> all_goods;              // set of all goods 0 to m-1
        vector<vector<ld>> bb_ratios;         // BB ratios matrix: n x m 
        vector<vector<ll>> allocation;        // allocation[i] contains goods allocated to agent i
        vector<ld> prices;                    // prices of goods

        
        const ll PRICEORDER;                  // large number L

        struct Edge
        {
            ll from, to;
            Edge(ll f, ll t)
            {
                from = f;
                to = t;
            }
        };

        // helper functions
        ld exponentiation(ll base, ll exp)
        {
            ld res = 1;
            while (exp > 0)
            {
                if (res > numeric_limits<double>::max() / base)
                {
                    cout << BLUE << "ld_MAX hit in prices" << RESET;

                    ld scaling_factor = base;
                    for (ld& p : prices)
                        p /= scaling_factor;

                    res /= scaling_factor;
                }
                res *= base;
                exp--;
            }
            return res;
        }

        ld calc_agent_spending(ll agent)
        {
            ld bundle_price = 0.0;
            for (auto good : allocation[agent])
                bundle_price += prices[good];

            return bundle_price;
        }

        ld calc_agent_utility(ll agent)
        {
            ld utility = 0;
            for (auto good : allocation[agent])
                utility += vals[agent][good];

            return utility;
        }

        inline ld get_MBB_ratio (ll agent)
        {
            return *max_element(bb_ratios[agent].begin(), bb_ratios[agent].end());
        }


        // check if agent i pEF1 envies agent j
        // will return true is j is pEF1 violator for i
        bool is_pEF1_violator(ll i, ll j)
        {       
            if (i==j)   return false;
            ld spending_i = calc_agent_spending(i), spending_j = calc_agent_spending(j);
            
            ld max_priced_good_j = 0.0;
            for (auto good : allocation[j])
                max_priced_good_j = max(max_priced_good_j, prices[good]);
            
            // p(x_j \ good) > p(x_i) which means j is a pEF1 violator for i
            return PRICE_EPSILON < (spending_j - max_priced_good_j - spending_i);
        }
        
        // return true if allocation is pEF1
        bool is_alloc_pEF1()
        {        
            for (ll i = 0; i < n; i++)
            {
                for (ll j = 0; j < n; j++)
                {
                    if (is_pEF1_violator(i, j)) return false;
                }
            }
            return true;
        }

        void set_initial_alloc()
        {
            // start with empty allocation
            for (ll i = 0; i < n; i++)
                allocation[i].clear();

            for (ll j = 0; j < m; j++)
            {
                vector<ll> agents;
                for (ll i = 0; i < n; i++)
                {
                    if (vals[i][j] > 0)
                        agents.push_back(i);
                }

                ll max_agent = max(agents[0], agents[1]);
                allocation[max_agent].push_back(j);

                // set initial price
                prices[j] = pow(PRICEORDER, max_agent-1);
                ld scaling_factor = vals[max_agent][j];
                if (prices[j] > numeric_limits<double>::max() / vals[max_agent][j])
                {
                    for (ld& p : prices)
                        p /= scaling_factor;
                }
                prices[j] *= scaling_factor;
            }
        }

        void set_bb_ratios()
        {
            for (ll i = 0; i < n; i++)
            {
                for (ll j = 0; j < m; j++)
                    bb_ratios[i][j] = static_cast<ld>(vals[i][j]) / prices[j];
            }
        }
        
        // get MBB edges for an agent
        vector<Edge> get_MBB_edges(ll agent)
        {
            vector<Edge> mbb_edges;
            ld mbb_ratio = get_MBB_ratio(agent);
            // get all edges with max ratio
            for (ll j = 0; j < m; j++)
            {
                if (abs(bb_ratios[agent][j] - mbb_ratio) < MBB_EPSILON)
                    mbb_edges.push_back(Edge(agent, j));
            }
            
            return mbb_edges;
        }

        vector<ll> get_ls()
        {
            vector<ld> agent_spendings(n, 0);
            for (ll i = 0; i < n; i++)
                agent_spendings[i] = calc_agent_spending(i);
            
            ld min_spending = *min_element(agent_spendings.begin(), agent_spendings.end());

            vector<ll> L;  // set of ls
            for (ll i = 0; i < n; i++) {
                if (abs(agent_spendings[i] - min_spending) < PRICE_EPSILON)
                    L.push_back(i);
            }
            return L;
        }
        
        // get C_L (component of L) = union of hierarchies/components of all ls
        set<ll> get_component(vector<ll> L)
        {
            set<ll> CL;

            for (ll i : L)
            {
                vector<set<ll>> hierarchy = build_hierarchy(i);

                for (auto l : hierarchy)
                    CL.insert(l.begin(), l.end());
            }
            return CL;
        }

        // get price rise factor beta = min(gamma1, gamma2)
        ld get_price_rise_factor(vector<ll> L, set<ll> CL)
        {
            ld gamma1 = numeric_limits<double>::max(), gamma2 = numeric_limits<double>::max();
            set<ll> diff_goods, CL_goods;

            for (ll j = 0; j < m; j++)
                diff_goods.insert(j);

            for (ll i : CL)
            {
                for (ll good : allocation[i])
                {
                    CL_goods.insert(good);
                    diff_goods.erase(good);
                }
                    
            }

            // set_difference(all_goods.begin(), all_goods.end(), CL_goods.begin(), CL_goods.end(),
            //                inserter(diff_goods, diff_goods.end()));

            // gamma1 = min ratio until new MBB edge appears
            for (ll h : CL)
            {
                ld mbb_ratio = get_MBB_ratio(h);
                for (ll j : diff_goods)
                {
                    ld cur_ratio = vals[h][j] / prices[j];
                    if (cur_ratio > 0)
                        gamma1 = min(gamma1, mbb_ratio / cur_ratio);
                }
            }
            
            // gamma2 = min ratio until new agent enters L
            for (ll i : L)
            {
                ld spending_i = calc_agent_spending(i);
                for (ll h=0; h<n; h++)
                {
                    if (find(CL.begin(), CL.end(), h) == CL.end())
                    {
                        ld spending_h = calc_agent_spending(h);
                        if (spending_i > 0)
                            gamma2 = min(gamma2, spending_h / spending_i);
                    }
                }
            }
            return min(gamma1, gamma2);
        }
        
        // build hierarchy starting from agent i
        vector<set<ll>> build_hierarchy(ll i)
        {
            // hierarchy only contains agents at each level (not the corresponding goods)
            vector<set<ll>> hierarchy;
            hierarchy.push_back({i});

            ll l = 0;
            while (!hierarchy[l].empty())
            {
                set<ll> next_level;
                for (ll agent : hierarchy[l])
                {
                    vector<Edge> agent_mbb_edges = get_MBB_edges(agent);
                    for (auto edge : agent_mbb_edges)
                    {
                        // good corresponding to this mbb edge
                        ll mbb_good = edge.to;
                        // agent that the mbb_good is allotted to (allocation edge)
                        ll next_agent = -1;
                        for (ll i=0; i<n; i++)
                        {
                            vector<ll> temp = allocation[i];
                            if (find(temp.begin(), temp.end(), mbb_good) != temp.end())
                            {
                                next_agent = i;
                                break;
                            }
                        }
                        if (next_agent == -1 && next_agent!=(agent+1)%n && next_agent!=(agent-1)%n)   break;

                        // ensure that agent is not already present in hierarchy (at some other level)
                        bool already_in_hierarchy = false;
                        for (auto level : hierarchy) {
                            if (level.find(next_agent) != level.end())
                            {
                                already_in_hierarchy = true;
                                break;
                            }
                        }            
                        if (!already_in_hierarchy)
                            next_level.insert(next_agent);
                    }
                }
                if (next_level.empty()) break;
                hierarchy.push_back(next_level); 
                l++;
            }

            return hierarchy;
        }

        // get altnernating path from agent i to a path violator
        vector<ll> get_alternating_path(ll i)
        {
            // cout << "\nbldg hier\n";
            vector<set<ll>> hierarchy = build_hierarchy(i);
            // for (auto x : hierarchy)
            // {
            //     for (auto s : x)
            //         cout << s << " ";
            //     cout << endl;
            // }
            // cout << "Built hierarchy\n";
            ld spending_i = calc_agent_spending(i);
            vector<ll> path;
            for (size_t level = 1; level < hierarchy.size(); level++)
            {
                // cout << "\nlevel: " << level << endl;
                for (auto h : hierarchy[level])
                {
                    vector<ll> alloc_h = allocation[h];
                    ld spending_h = calc_agent_spending(h);

                    for (auto j : alloc_h)
                    {
                        for (auto k : hierarchy[level-1])
                        {
                            ld mbb_ratio_k = get_MBB_ratio(k);
                            // cout << "\ninside alt path calc" << ld(spending_h-prices[j]-spending_i);
                            if (abs(bb_ratios[k][j] - mbb_ratio_k) < MBB_EPSILON && spending_h - prices[j] - spending_i > PRICE_EPSILON)
                            {
                                // good j (belonging to h) is MBB for k, so k--j--h is an alternating path
                                // h is a path violator wrt good j
                                // we only return k, j, h becasue need to transfer j from h to k

                                // cout << "testing" << k << " " << j << " " << h << " " << level << endl;
                                return {k, j, h, ll(level)};
                            }
                        }
                    }
                }
            }

            return {};
        }

    public:
        // class constructor
        EF1POAllocation(ll n, ll m, ll price_order, const vector<vector<ll>>& valuations)
        : n(n), m(m), PRICEORDER(price_order), vals(valuations)
        {
            prices = vector<ld>(m, 1.0);
            allocation = vector<vector<ll>>(n);
            bb_ratios = vector<vector<ld>>(n, vector<ld>(m, 0.0));
        }

        // main function to compute the allocation
        vector<vector<ll>> compute_allocation()
        {
            set_initial_alloc();
            set_bb_ratios();

            cout << "Initial prices: ";
            cout_vector(prices);
            // cout << "\nbb ratios: ";
            // cout_matrix(bb_ratios);
            cout<<"Initial done\n";

            ll cnt = 0;

            while (!is_alloc_pEF1()) {
                cout << "\niteration " << ++cnt << endl;

                vector<ll> L = get_ls();
                cout << "least spenders: ";
                cout_vector(L);
                // cout << endl;

                // cout << "Agent spendings: ";
                // for (int i=0; i<n; i++)
                //     cout << calc_agent_spending(i) << " ";
                // cout << endl;

                ll min_ls = -1, min_level = LLONG_MAX;
                vector<ll> min_ls_path;
                for (ll ls : L)
                {
                    // cout << ls;
                    vector<ll> ls_path = get_alternating_path(ls);
                    // cout << ls << "ls path ";
                    // cout_vector(ls_path);
                    // cout << endl;
                    if (!ls_path.empty())
                    {
                        // cout << "path not empty\n";
                        if (min_level > ls_path.back())
                        {
                            min_level = ls_path.back();
                            min_ls = ls;
                            min_ls_path = ls_path;
                            // cout << min_level << endl;
                            // cout_vector(min_ls_path);
                            // cout << "path printed\n";
                        }
                    }
                }

                auto CL = get_component(L);

                if (min_ls == -1)
                {
                    // for each ls in L, there is no path violator (hence ls_path are all empty and min_ls=-1)
                    // price rise step
                    cout << GREEN << "price rise\n" << RESET;
                    ld beta = get_price_rise_factor(L, CL);
                    cout << setprecision(21) << "beta (price rise factor): " << beta << endl;

                    for (ll i : CL)
                    {
                        for (ll good : allocation[i])
                            prices[good] *= beta;
                    }
                    set_bb_ratios();
                }
                else
                {
                    // transfer step
                    cout << GREEN << "transfer\n" << RESET;
                    
                    ll k = min_ls_path[0], j = min_ls_path[1], h = min_ls_path[2];
                    cout << "transfer good " << j << " from agent " << h << " to agent " << k << endl;

                    allocation[k].push_back(j);
                    vector<ll>& alloc_h = allocation[h];
                    alloc_h.erase(remove(alloc_h.begin(), alloc_h.end(), j), alloc_h.end());
                }
                // cout << endl;
                // for (int i=0; i<n; i++)
                //     cout << calc_agent_spending(i) << " ";
                // cout << endl;
            }
            return allocation;
        }

        // get prices
        vector<ld> get_prices()
        {
            return prices;
        }
};

ll compute_price_order(ll n, const vector<vector<ll>>& vals)
{
    ll v_max = LLONG_MIN;
    for (const auto row : vals)
    {
        auto row_max = *max_element(row.begin(), row.end());
        v_max = max(v_max, row_max);
    }
    // following is computed to ensure that a0 has a1 for mbb even in small case of n=3 where a2 price order is just L
    return 1 + ((n==3)?(v_max*v_max):(v_max));
}

vector<vector<ll>> generate_random_vals_matrix(ll n, ll m)
{
    // srand(time(0));
    vector<vector<ll>> matrix(n, vector<ll>(m, 0));

    // get number of edges (stored in cols) between each of the n pairs of agents
    ll sub_sum = m-n;
    vector<ll> cols(n, 1);
    for (ll i = 0; i < n-1; i++)
    {
        if (sub_sum <= 0) break;
        ll temp = rand() % (sub_sum); 
        cols[i] += temp;
        sub_sum -= temp;
    }
    cols[n-1] += sub_sum;
    
    cout_vector(cols);
    
    // generate random values for each good
    ll row1 = 0, row2 = 1, g = 0;
    for (ll i = 0; i < n; i++) {
        for (ll j = 0; j < cols[i]; j++)
        {
            matrix[row1][g+j] = rand() % MAX_VAL + 1;
            matrix[row2][g+j] = rand() % MAX_VAL + 1;
        }
        g+= cols[i];
        row1++;
        row2 = (row2+1)%n;
    }

    return matrix;
}

vector<vector<ll>> get_input(ll& n, ll& m)
{
    cout << "Enter number of agents: ";
    cin >> n;
    cout << "Enter number of goods: ";
    cin >> m;
    vector<vector<ll>> vals(n, vector<ll>(m, 0));
    cout << "Enter valuation matrix (including 0s):\n";
    for (ll i = 0; i < n; i++)
    {
        for (ll j = 0; j < m; j++)
            cin >> vals[i][j];
    }
    return vals;
}

int main()
{
    // setup
    auto start = chrono::high_resolution_clock::now();
    // srand(time(0));
    cout << boolalpha;

    // input
    ll n, m;
    n=10;
    m=500;
    assert(m>=n);
    
    vector<vector<ll>> vals = generate_random_vals_matrix(n,m);
    // print valuation matrix
    cout << "Valuation matrix:\n";
    cout_matrix(vals);

    // auto vals = get_input(n,m);


    // compute L (order of prices)
    ll price_order = compute_price_order(n, vals);
    cout << "price order: " << price_order << endl;

    // create instance and compute allocation
    EF1POAllocation instance = EF1POAllocation(n, m, price_order, vals);
    auto final_alloc = instance.compute_allocation();
    cout << endl;

    // get final prices and max price
    auto prices = instance.get_prices();
    cout_vector(prices);
    cout << "\nmax price: " << *max_element(prices.begin(), prices.end()) << endl;

    // final allocation represented as (Agent i : list of goods assigned to agent i)
    cout << "\nFinal allocation:\n";
    for (ll i=0; i<n; i++)
    {
        cout << "Agent " << i << ": ";
        for (ll j=0; j<final_alloc[i].size(); j++)
            cout << final_alloc[i][j] << " ";
        cout << "\n";
    }

    // compute execution time
    auto end = chrono::high_resolution_clock::now();
    chrono::duration<ld> duration = end - start;
    cout << "Execution time: " << duration.count() << " seconds";
    
    return 0;
}
#include <map>
#include <list>
#include <set>
#include <iostream>
#include <sstream>
#include <iterator>
#include <fstream>
#include <vector>

using namespace std;

pair<string, list<string> > parse_prod(string s) {
    stringstream str(s);
    string lhs;
    str >> lhs;
    istream_iterator<string> it(str), end;
    list<string> rhs(it,end);
    return pair<string, list<string> >(lhs,rhs);
}

// handy debugging routine.
string slice(string s, int i, int j) {
    if (j < i) {
        return "";
    }
    return s.substr(i,j-i+1);
}

// handy debugging output things;
ostream& operator<<(ostream& o, set<string> s) {
    o << "{ ";
    for (auto it = s.begin(); it != s.end(); ++it) {
        o << *it << " ";
    }
    o << "}";
    return o;
}


template<class iter>
unsigned length(iter begin, iter end) {
    unsigned n = 0;
    for (; begin != end; ++begin) {
        ++n;rod "S" "A" "B",
NTprod "A" "A" "A",
Tprod "A" "a",
NTprod "B" "B" "B",

    }
    return n;
}


template<class S>
class cyktable {
    private:
    vector<vector<set<S>>> M;
    public:
    cyktable(unsigned n): M(n) {
        for (unsigned i = 0; i < n; ++i) { M[i] = vector<set<S>>(n); }
    }
    vector<set<S>>& operator[](int i) { return M[i]; }
    unsigned size() const { return M.size(); }
};


template<class S>
ostream& operator<<(ostream& o, cyktable<S>& m) {
    for (unsigned i = 0; i < m.size(); ++i) {
        for (unsigned j = 0; j < m[i].size(); ++j) {
            o << i << ", " << j << " : " << m[i][j] << endl;
        }
    }
    return o;
}


list<string> make_unit_strings(string s) {
    list<string> l;
    for (auto it = s.begin(); it != s.end(); ++it) {
        string r = "";
        r += *it;
        l.push_back(r);
    }
    return l;
}



template<class S>
class cfg {
    multimap<S, list<S>> productions;
    multimap<list<S>, S> reverse_prod;

    public:
    const S start_symb;
    cfg(): start_symb("S") {}

    void insert(pair<S, list<S>> p) {
        productions.insert(p);
        reverse_prod.insert(pair<list<S>,S>(p.second, p.first));
    }

    // returns all symbols that generate the provided RHS.
    set<S> generators(initializer_list<S> input) {
        list<S> ind(input.begin(), input.end());
        auto range = reverse_prod.equal_range(ind);
        set<S> s;
        for (auto it = range.first; it != range.second; ++it) {
            s.insert(it->second);
        }
        return s;
    }

};



template<class S, class iter>
bool cyk(cfg<S> g, unsigned n, iter seq_begin, iter seq_end) {
    cyktable<S> M(n);

    for (unsigned i = 0; i < n && seq_begin != seq_end; ++i, ++seq_begin) {
        M[i][i] = g.generators({*seq_begin});
    }

    for (unsigned l = 1; l < n; ++l) { // for each length
        for (unsigned r = 0; r < n-l; ++r) { // for each start
            // the string we're considering
            for (unsigned t = 0; t < l; ++t) { // for each split-point
                // what generates the two halves of this split?
                auto lhs = M[r][r+t];
                auto rhs = M[r+t+1][r+l];
                for (auto it = lhs.begin(); it != lhs.end(); ++it) {
                    for (auto jt = rhs.begin(); jt != rhs.end(); ++jt) {
                        // "gens" is the set of all nonterminals generating itjt.
                        auto gens = g.generators({*it,*jt});
                        M[r][r+l].insert(gens.begin(),gens.end());
                    }
                }
            }
        }
    }
    return M[0][n-1].find(g.start_symb) != M[0][n-1].end();
}


int main(int argc, char* argv[]) {
    fstream f;
    f.open(argv[1]);
    string line;
    cfg<string> g;
    while(getline(f,line)) {
        pair<string, list<string>> prod = parse_prod(line);
        g.insert(prod);
    }
    string s(argv[2]);

    // the input is just one ``word'', but we want to treat each character
    // as a token, and in our algorithm tokens are strings.
    list<string> l = make_unit_strings(s);
    if(cyk(g, l.size(), l.begin(), l.end()))
        cout<< "true"<< endl;
    else
        cout<< "false"<< endl;
}



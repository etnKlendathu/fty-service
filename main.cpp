#include "services.h" // <- do not look into this, this is just concept, should be cleaned.
#include <thread>

// =========================================================================================================================================

struct Msg : public pack::Node
{
    pack::String fld = FIELD("fied");

    using pack::Node::Node;
    META(Msg, fld);
};

// =========================================================================================================================================

// Universal declaration of the service, common for client and server
SERVICE(Service)
{
    CLIENT_NAME("my-client"); // <- client agent name
    SERVER_NAME("my-server"); // <- server agent name
    CHANNEL_NAME("my-comm");  // <- Channel name

    METHOD(call, Msg(const Msg&, bool)) = SUBJECT("call-me"); // Function declaration as (function name, signature) = subject

    SERVICE_META(Service, call); // Same as in pack
};

// =========================================================================================================================================

class Dispatcher
{
public:
    static fty::Expected<Dispatcher> create(const std::string& endpoint)
    {
        if (auto ret = Service<Server>::create(endpoint)) {
            return fty::Expected<Dispatcher>(Dispatcher(std::move(*ret)));
        } else {
            return fty::unexpected(ret.error());
        }
    }

private:
    Msg onCall(const Msg& msg, bool val)
    {
        // Here we process our call :)
        // All params are unpacked, and ready to use
        Msg ret;
        ret.fld = fmt::format("Process {}, {} -> {}", msg.fld.value(), val, ": Ha-ha-ha");

        // Just return your output... simple
        return ret;
    }

private:
    Dispatcher(Service<Server>&& serv)
        : m_serv(std::move(serv))
    {
        // just connect YOUR process function to service callback
        m_serv.call.connect(&Dispatcher::onCall, this);
    }
    Service<Server> m_serv;
};

// =========================================================================================================================================

int main(int /*argc*/, char** /*argv*/)
{
    static std::string endpoint = "inproc://test-agent";

    zactor_t* malamute = zactor_new(mlm_server, const_cast<char*>("Malamute"));
    zstr_sendx(malamute, "BIND", endpoint.c_str(), NULL);


    // Create server part... for pretty usage wrapped into the Dispatcher class
    fty::Expected<Dispatcher> dis = Dispatcher::create(endpoint);

    std::thread th([&]() {
        // Create client
        fty::Expected<Service<Client>> cln = Service<Client>::create(endpoint);
        if (cln) {
            Msg msg;
            msg.fld  = "my field";

            // Call as regular member function :D
            auto ret = cln->call(msg, true);
            if (!ret) {
                std::cerr << "Call error:" << ret.error() << std::endl;
            } else {
                std::cerr << fmt::format("Call result: {}", *ret) << std::endl;
            }
        }
    });

    th.join();

    zactor_destroy(&malamute);
    return EXIT_SUCCESS;
}
